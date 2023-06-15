import Tricks: static_fieldnames, static_fieldtypes

const DEFAULT_DIM_TYPE = FixedRational{Int32, 2^4 * 3^2 * 5^2 * 7}
const DEFAULT_VALUE_TYPE = Float64

abstract type AbstractQuantity{T,R} end
abstract type AbstractDimensions{R} end

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

(::Type{D})(::Type{R}; kws...) where {R,D<:AbstractDimensions} = D{R}((tryrationalize(R, get(kws, k, zero(R))) for k in static_fieldnames(D))...)
(::Type{D})(; kws...) where {D<:AbstractDimensions} = D(DEFAULT_DIM_TYPE; kws...)

(::Type{D})(args...) where {R,D<:AbstractDimensions{R}} = dimension_constructor(D)(Base.Fix1(convert, R).(args)...)
(::Type{D})(; kws...) where {R,D<:AbstractDimensions{R}} = dimension_constructor(D)(R; kws...)
(::Type{D})(d::AbstractDimensions) where {R,D<:AbstractDimensions{R}} = D((getproperty(d, k) for k in static_fieldnames(D))...)

new_dimensions(::Type{QD}, dims...) where {QD<:Union{AbstractQuantity,AbstractDimensions}} = dimension_constructor(QD)(dims...)


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
end

(::Type{Q})(x, ::Type{R}; kws...) where {R,Q<:AbstractQuantity} = quantity_constructor(Q){typeof(x), R}(x, dimension_constructor(Q)(R; kws...))
(::Type{Q})(x; kws...) where {Q<:AbstractQuantity} = Q(x, DEFAULT_DIM_TYPE; kws...)
(::Type{Q})(q::AbstractQuantity) where {T,Q<:AbstractQuantity{T}} = new_quantity(Q, convert(T, ustrip(q)), dimension(q))
(::Type{Q})(q::AbstractQuantity) where {T,R,Q<:AbstractQuantity{T,R}} = new_quantity(Q, convert(T, ustrip(q)), dimension_constructor(Q){R}(dimension(q)))

new_quantity(::Type{QD}, l, r) where {QD<:Union{AbstractQuantity,AbstractDimensions}} = quantity_constructor(QD)(l, r)

function container_type(::Type{T}) where {T}
    return Base.typename(T).wrapper
end
@generated function get_dim_type(::Type{Q}) where {Q<:AbstractQuantity}
    quantity_type = container_type(Q)
    field_type = NamedTuple(static_fieldnames(quantity_type) .=> static_fieldtypes(quantity_type))[:dimensions]
    out = container_type(field_type)
    return :($out)
end

"""
    dimension_constructor(::Type{<:AbstractDimensions})

This function returns the container for a particular `AbstractDimensions`.
For example, `Dimensions` will get returned as `Dimensions`, and
`Dimensions{Rational{Int64}}` will also get returned as `Dimensions`.
"""
dimension_constructor(::Type{D}) where {D<:AbstractDimensions} = container_type(D)

"""
    dimension_constructor(::Type{<:AbstractQuantity})

This function returns the `Dimensions` type used inside
a particular `Quantity` type by reading the `.dimensions` field.
It also strips the type parameter (i.e., `Dimensions{R} -> Dimensions`).
"""
dimension_constructor(::Type{Q}) where {Q<:AbstractQuantity} = get_dim_type(Q)

"""
    quantity_constructor(::Type{<:AbstractQuantity})

This function returns the container for a particular `AbstractQuantity`.
For example, `Quantity` gets returned as `Quantity`, `Quantity{Float32}` also
as `Quantity`, and `Quantity{Float32,Rational{Int64}}` also as `Quantity`.
"""
quantity_constructor(::Type{Q}) where {Q<:AbstractQuantity} = container_type(Q)

"""
    quantity_constructor(::Type{<:AbstractDimensions})

This function returns the `<:AbstractQuantity` type corresponding to
a given `<:AbstractDimensions`. For example, `Dimensions -> Quantity`.
If you define a custom dimensions type, you should overload this function
so it returns your custom quantity type that uses that dimensions type.
This is only needed if you wish to use the `(*)(::AbstractDimensions, ::Number)`
function; otherwise it won't be necessary.
"""
quantity_constructor(::Type{D}) where {D<:Dimensions} = Quantity

struct DimensionError{Q1,Q2} <: Exception
    q1::Q1
    q2::Q2
end
