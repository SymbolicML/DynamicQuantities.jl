import Tricks: static_fieldnames, static_fieldtypes

const DEFAULT_DIM_BASE_TYPE = FixedRational{DEFAULT_NUMERATOR_TYPE,DEFAULT_DENOM}
const DEFAULT_VALUE_TYPE = Float64

"""
    AbstractDimensions{R}

An abstract type for dimension types. `R` is the type of the exponents of the dimensions,
and by default is set to `DynamicQuantities.DEFAULT_DIM_BASE_TYPE`.
AbstractDimensions are used to store the dimensions of `AbstractQuantity` objects.
Together these enable many operators in Base to manipulate dimensions.
This type has generic constructors for creating dimension objects, so user-defined
dimension types can be created by simply subtyping `AbstractDimensions`, without
the need to define many other functions.

The key function that one could wish to overload is
`DynamicQuantities.dimension_name(::AbstractDimensions, k::Symbol)` for mapping from a field name
to a base unit (e.g., `length` by default maps to `m`). You may also need to overload
`DynamicQuantities.constructor_of(::Type{T})` in case of non-standard construction.
"""
abstract type AbstractDimensions{R} end

"""
    AbstractQuantity{T,D}

An abstract type for quantities. `T` is the type of the value of the quantity,
and `D` is the type of the dimensions of the quantity. By default, `D` is set to
`DynamicQuantities.DEFAULT_DIM_TYPE`. `T` is inferred from the value in a calculation,
but in other cases is defaulted to `DynamicQuantities.DEFAULT_VALUE_TYPE`.
It is assumed that the value is stored in the `:value` field, and the dimensions
object is stored in the `:dimensions` field. These fields can be accessed with
`ustrip` and `dimension`, respectively. Many operators in `Base` are defined on
`AbstractQuantity` objects, including `+, -, *, /, ^, sqrt, cbrt, abs`.
"""
abstract type AbstractQuantity{T,D} end

"""
    Dimensions{R<:Real} <: AbstractDimensions{R}

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

- `Dimensions(args...)`: Pass all the dimensions as arguments. `R` is set to `DEFAULT_DIM_BASE_TYPE`.
- `Dimensions(; kws...)`: Pass a subset of dimensions as keyword arguments. `R` is set to `DEFAULT_DIM_BASE_TYPE`.
- `Dimensions(::Type{R}; kws...)` or `Dimensions{R}(; kws...)`: Pass a subset of dimensions as keyword arguments, with the output type set to `Dimensions{R}`.
- `Dimensions{R}()`: Create a dimensionless object typed as `Dimensions{R}`.
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

(::Type{D})(::Type{R}; kws...) where {R,D<:AbstractDimensions} = constructor_of(D){R}((tryrationalize(R, get(kws, k, zero(R))) for k in static_fieldnames(D))...)
(::Type{D})(; kws...) where {R,D<:AbstractDimensions{R}} = constructor_of(D)(R; kws...)
(::Type{D})(; kws...) where {D<:AbstractDimensions} = D(DEFAULT_DIM_BASE_TYPE; kws...)
(::Type{D})(d::AbstractDimensions) where {R,D<:AbstractDimensions{R}} = D((getproperty(d, k) for k in static_fieldnames(D))...)

const DEFAULT_DIM_TYPE = Dimensions{DEFAULT_DIM_BASE_TYPE}

"""
    Quantity{T,D<:AbstractDimensions} <: AbstractQuantity{T,D}

Physical quantity with value `value` of type `T` and dimensions `dimensions` of type `D`.
For example, the velocity of an object with mass 1 kg and velocity
2 m/s is `Quantity(2, mass=1, length=1, time=-1)`.
You should access these fields with `ustrip(q)`, and `dimension(q)`.
You can access specific dimensions with `ulength(q)`, `umass(q)`, `utime(q)`,
`ucurrent(q)`, `utemperature(q)`, `uluminosity(q)`, and `uamount(q)`.

Severals operators in `Base` are extended to work with `Quantity` objects,
including `*`, `+`, `-`, `/`, `abs`, `^`, `sqrt`, and `cbrt`, which manipulate
dimensions according to the operation.

# Fields

- `value::T`: value of the quantity of some type `T`. Access with `ustrip(::Quantity)`
- `dimensions::D`: dimensions of the quantity. Access with `dimension(::Quantity)`

# Constructors

- `Quantity(x; kws...)`: Construct a quantity with value `x` and dimensions given by the keyword arguments. The value
   type is inferred from `x`. `R` is set to `DEFAULT_DIM_TYPE`.
- `Quantity(x, ::Type{D}; kws...)`: Construct a quantity with value `x` with dimensions given by the keyword arguments,
   and the dimensions type set to `D`.
- `Quantity(x, d::D)`: Construct a quantity with value `x` and dimensions `d` of type `D`.
- `Quantity{T}(...)`: As above, but converting the value to type `T`. You may also pass a `Quantity` as input.
- `Quantity{T,D}(...)`: As above, but converting the value to type `T` and dimensions to `D`. You may also pass a
  `Quantity` as input.
"""
struct Quantity{T,D<:AbstractDimensions} <: AbstractQuantity{T,D}
    value::T
    dimensions::D

    Quantity(x::_T, dimensions::_D) where {_T,_D<:AbstractDimensions} = new{_T,_D}(x, dimensions)
end
(::Type{Q})(x::T, ::Type{D}; kws...) where {D<:AbstractDimensions,T,T2,Q<:AbstractQuantity{T2}} = constructor_of(Q)(convert(T2, x), D(; kws...))
(::Type{Q})(x, ::Type{D}; kws...) where {D<:AbstractDimensions,Q<:AbstractQuantity} = constructor_of(Q)(x, D(; kws...))
(::Type{Q})(x::T; kws...) where {T,T2,Q<:AbstractQuantity{T2}} = constructor_of(Q)(convert(T2, x), dim_type(Q)(; kws...))
(::Type{Q})(x; kws...) where {Q<:AbstractQuantity} = constructor_of(Q)(x, dim_type(Q)(; kws...))

(::Type{Q})(q::AbstractQuantity) where {T,D<:AbstractDimensions,Q<:AbstractQuantity{T,D}} = constructor_of(Q)(convert(T, ustrip(q)), convert(D, dimension(q)))
(::Type{Q})(q::AbstractQuantity) where {T,Q<:AbstractQuantity{T}} = constructor_of(Q)(convert(T, ustrip(q)), dimension(q))

const DEFAULT_QUANTITY_TYPE = Quantity{DEFAULT_VALUE_TYPE, DEFAULT_DIM_TYPE}

new_dimensions(::Type{D}, dims...) where {D<:AbstractDimensions} = constructor_of(D)(dims...)
new_quantity(::Type{Q}, l, r) where {Q<:AbstractQuantity} = constructor_of(Q)(l, r)

dim_type(::Type{Q}) where {T,D<:AbstractDimensions,Q<:AbstractQuantity{T,D}} = D
dim_type(::Type{<:AbstractQuantity}) = DEFAULT_DIM_TYPE
constructor_of(::Type{T}) where {T} = Base.typename(T).wrapper

struct DimensionError{Q1,Q2} <: Exception
    q1::Q1
    q2::Q2
end
