const DEFAULT_QUANTITY_TYPE = Quantity

"""
    QuantityArray{T,N,D,V}

An array of quantities with value `value` of type `V` and dimensions `dimensions` of type `D` (
which are shared across all elements of the array). This is a subtype of `AbstractArray{T,N}`,
and so can be used in most places where a normal array would be used. The one caveat to note
is that while `getindex` returns `::Quantity`, `setindex!` expects a `::T`. This is done to
avoid unnecessary dimension checks. This is not performed automatically so that the user
is forced to be aware of it.

# Fields

- `value`: The underlying array of values.
- `dimensions`: The dimensions of the array.

# Constructors

- `QuantityArray(value::AbstractArray, dimensions::AbstractDimensions)`: Create a `QuantityArray` with value `value` and dimensions `dimensions`.
- `QuantityArray(value::AbstractArray, quantity::Quantity)`: Create a `QuantityArray` with value `value` and dimensions inferred 
   with `dimension(quantity)`. This is so that you can easily create an array with the units module, like so:
   ```julia
   julia> A = QuantityArray(randn(32), 1u"m")
   ```
- `QuantityArray(v::AbstractArray{<:AbstractQuantity})`: Create a `QuantityArray` from an array of quantities. This means the following
  syntax works:
  ```julia
  julia> A = QuantityArray(randn(32) .* 1u"km/s")
  ```
"""
struct QuantityArray{T,N,D<:AbstractDimensions,Q<:AbstractQuantity,V<:AbstractArray{T,N}} <: AbstractArray{Q,N}
    value::V
    dimensions::D

    QuantityArray(v::_V, d::_D) where {_T,_N,_D<:AbstractDimensions,_V<:AbstractArray{_T,_N}} = new{_T,_N,_D,DEFAULT_QUANTITY_TYPE{_T,DEFAULT_DIM_TYPE},_V}(v, d)
    QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity,_V<:AbstractArray{_T,_N}} = new{_T,_N,_D,_Q,_V}(v, d)
end

# Construct with a Quantity (easier, as you can use the units):
QuantityArray(v::AbstractArray; kws...) = QuantityArray(v, DEFAULT_DIM_TYPE(; kws...))
QuantityArray(v::AbstractArray, q::AbstractQuantity) = QuantityArray(v .* ustrip(q), dimension(q), typeof(q))
QuantityArray(v::QA) where {Q<:AbstractQuantity,QA<:AbstractArray{Q}} = allequal(dimension.(v)) ? QuantityArray(ustrip.(v), dimension(first(v)), Q) : throw(DimensionError(first(v), v))
# TODO: Should this check that the dimensions are the same?

ustrip(A::QuantityArray) = A.value
dimension(A::QuantityArray) = A.dimensions

array_type(::Type{A}) where {T,N,D,Q,V,A<:QuantityArray{T,N,D,Q,V}} = V
quantity_type(::Type{A}) where {T,N,D,Q,A<:QuantityArray{T,N,D,Q}} = Q
quantity_type(A) = quantity_type(typeof(A))

# One field:
for f in (:size, :length, :axes)
    @eval Base.$f(A::QuantityArray) = $f(ustrip(A))
end

Base.getindex(A::QuantityArray, i...) = quantity_type(A)(getindex(ustrip(A), i...), dimension(A))
Base.setindex!(A::QuantityArray{T,N,D,Q}, v::Q, i...) where {T,N,D,Q<:AbstractQuantity} = dimension(A) == dimension(v) ? unsafe_setindex!(A, v, i...) : throw(DimensionError(A, v))
# TODO: Should this dimension check be removed?
# TODO: This does not allow for efficient broadcasting; as the dimension calculation is repeated...

unsafe_setindex!(A, v, i...) = setindex!(ustrip(A), ustrip(v), i...)

Base.IndexStyle(::Type{Q}) where {Q<:QuantityArray} = IndexStyle(array_type(Q))
Base.similar(A::QuantityArray, args...) = QuantityArray(similar(ustrip(A), args...), dimension(A))
