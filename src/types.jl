const DEFAULT_DIM_TYPE = FixedRational{Int32, 2^4 * 3^2 * 5^2 * 7}
const DEFAULT_VALUE_TYPE = Float64

abstract type AbstractQuantity{T,R} end
abstract type AbstractDimensions{R} end

constructor_of(::Type{D}) where {D<:AbstractDimensions} = D
constructor_of(::Type{D}) where {R,D<:AbstractDimensions{R}} = D.name.wrapper
constructor_of(::Type{Q}) where {Q<:AbstractQuantity} = Q
constructor_of(::Type{Q}) where {T,Q<:AbstractQuantity{T}} = Q.body.name.wrapper
constructor_of(::Type{Q}) where {T,R,Q<:AbstractQuantity{T,R}} = Q.name.wrapper

"""
    Dimensions{R}

A type representing the dimensions of a quantity, with each
field giving the power of the corresponding dimension. For
example, the dimensions of velocity are `Dimensions(length=1, time=-1)`.
Each of the 7 dimensions are stored using the type `R`,
which is by default a rational number.

# Fields

- `length`: length dimension (i.e., meters^(length))
- `mass`: mass dimension (i.e., kg^(mass))
- `time`: time dimension (i.e., s^(time))
- `current`: current dimension (i.e., A^(current))
- `temperature`: temperature dimension (i.e., K^(temperature))
- `luminosity`: luminosity dimension (i.e., cd^(luminosity))
- `amount`: amount dimension (i.e., mol^(amount))

# Constructors

- `Dimensions(args...)`: Pass all the dimensions as arguments. `R` is set to `DEFAULT_DIM_TYPE`.
- `Dimensions(; kws...)`: Pass a subset of dimensions as keyword arguments. `R` is set to `DEFAULT_DIM_TYPE`.
- `Dimensions(::Type{R}; kws...)` or `Dimensions{R}(; kws...)`: Pass a subset of dimensions as keyword arguments, with the output type set to `Dimensions{R}`.
- `Dimensions{R}(args...)`: Pass all the dimensions as arguments, with the output type set to `Dimensions{R}`.
- `Dimensions{R}(d::Dimensions)`: Copy the dimensions from another `Dimensions` object, with the output type set to `Dimensions{R}`.

"""
struct Dimensions{R<:Real} <: AbstractDimensions{R}
    length::R
    mass::R
    time::R
    current::R
    temperature::R
    luminosity::R
    amount::R
end

(::Type{D})(::Type{R}; kws...) where {R,D<:AbstractDimensions} = D{R}((tryrationalize(R, get(kws, k, zero(R))) for k in fieldnames(D))...)
(::Type{D})(; kws...) where {D<:AbstractDimensions} = D(DEFAULT_DIM_TYPE; kws...)

(::Type{D})(args...) where {R,D<:AbstractDimensions{R}} = constructor_of(D)(Base.Fix1(convert, R).(args)...)
(::Type{D})(; kws...) where {R,D<:AbstractDimensions{R}} = constructor_of(D)(R; kws...)
(::Type{D})(d::AbstractDimensions) where {R,D<:AbstractDimensions{R}} = D((getfield(d, k) for k in fieldnames(D))...)


"""
    Quantity{T,R}

Physical quantity with value `value` of type `T` and dimensions `dimensions` of type `Dimensions{R}`.
For example, the velocity of an object with mass 1 kg and velocity
2 m/s is `Quantity(2, mass=1, length=1, time=-1)`.
You should access these fields with `ustrip(q)`, and `dimensions(q)`.
You can access specific dimensions with `ulength(q)`, `umass(q)`, `utime(q)`,
`ucurrent(q)`, `utemperature(q)`, `uluminosity(q)`, and `uamount(q)`.

Severals operators in `Base` are extended to work with `Quantity` objects,
including `*`, `+`, `-`, `/`, `abs`, `^`, `sqrt`, and `cbrt`, which manipulate
dimensions according to the operation.

# Fields

- `value::T`: value of the quantity of some type `T`. Access with `ustrip(::Quantity)`
- `dimensions::Dimensions{R}`: dimensions of the quantity with dimension type `R`. Access with `dimension(::Quantity)`

# Constructors

- `Quantity(x; kws...)`: Construct a quantity with value `x` and dimensions given by the keyword arguments. The value type is inferred from `x`. `R` is set to `DEFAULT_DIM_TYPE`.
- `Quantity(x, ::Type{R}; kws...)`: Construct a quantity with value `x`. The dimensions parametric type is set to `R`.
- `Quantity(x, d::Dimensions{R})`: Construct a quantity with value `x` and dimensions `d`.
- `Quantity{T}(q::Quantity)`: Construct a quantity with value `q.value` and dimensions `q.dimensions`, but with value type converted to `T`.
- `Quantity{T,R}(q::Quantity)`: Construct a quantity with value `q.value` and dimensions `q.dimensions`, but with value type converted to `T` and dimensions parametric type set to `R`.
"""
struct Quantity{T,R} <: AbstractQuantity{T,R}
    value::T
    dimensions::Dimensions{R}

    Quantity(x; kws...) = new{typeof(x), DEFAULT_DIM_TYPE}(x, Dimensions(; kws...))
    Quantity(x, ::Type{_R}; kws...) where {_R}  = new{typeof(x), _R}(x, Dimensions(_R; kws...))
    Quantity(x, d::Dimensions{_R}) where {_R}  = new{typeof(x), _R}(x, d)
    Quantity{T}(q::Quantity) where {T} = Quantity(convert(T, q.value), dimension(q))
    Quantity{T,R}(q::Quantity) where {T,R} = Quantity(convert(T, q.value), Dimensions{R}(dimension(q)))
end

# All that is needed to make an `AbstractQuantity` work:
dimension_name(::Dimensions, k::Symbol) = (length="m", mass="kg", time="s", current="A", temperature="K", luminosity="cd", amount="mol")[k]
new_dimensions(::Type{<:Dimensions}, dims...) = Dimensions(dims...)
new_quantity(::Type{<:Union{<:Quantity,<:Dimensions}}, l, r) = Quantity(l, r)

struct DimensionError{Q1,Q2} <: Exception
    q1::Q1
    q2::Q2
end
