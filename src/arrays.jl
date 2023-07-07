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
struct QuantityArray{T,N,D<:AbstractDimensions,Q<:AbstractQuantity{T,D},V<:AbstractArray{T,N}} <: AbstractArray{Q,N}
    value::V
    dimensions::D

    QuantityArray(v::_V, d::_D) where {_T,_N,_D<:AbstractDimensions,_V<:AbstractArray{_T,_N}} = new{_T,_N,_D,DEFAULT_QUANTITY_TYPE{_T,_D},_V}(v, d)
    QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity{_T,_D},_V<:AbstractArray{_T,_N}} = new{_T,_N,_D,_Q,_V}(v, d)
    QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity{_T},_V<:AbstractArray{_T,_N}} = QuantityArray(v, d, constructor_of(_Q){_T,_D})
    QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity,_V<:AbstractArray{_T,_N}} = QuantityArray(v, d, _Q{_T,_D})
end

# Construct with a Quantity (easier, as you can use the units):
QuantityArray(v::AbstractArray; kws...) = QuantityArray(v, DEFAULT_DIM_TYPE(; kws...))
QuantityArray(v::AbstractArray, q::AbstractQuantity) = QuantityArray(v .* ustrip(q), dimension(q), typeof(q))
QuantityArray(v::QA) where {Q<:AbstractQuantity,QA<:AbstractArray{Q}} = allequal(dimension.(v)) ? QuantityArray(ustrip.(v), dimension(first(v)), Q) : throw(DimensionError(first(v), v))
# TODO: Should this check that the dimensions are the same?

ustrip(A::QuantityArray) = A.value
dimension(A::QuantityArray) = A.dimensions

array_type(::Type{A}) where {T,A<:QuantityArray{T}} = Array{T,1}
array_type(::Type{A}) where {T,N,A<:QuantityArray{T,N}} = Array{T,N}
array_type(::Type{A}) where {T,N,D,Q,V,A<:QuantityArray{T,N,D,Q,V}} = V

quantity_type(::Type{A}) where {T,N,D,Q,A<:QuantityArray{T,N,D,Q}} = Q
quantity_type(A) = quantity_type(typeof(A))

dim_type(::Type{A}) where {A<:QuantityArray} = DEFAULT_DIM_TYPE
dim_type(::Type{A}) where {T,N,D,A<:QuantityArray{T,N,D}} = D

# One field:
for f in (:size, :length, :axes)
    @eval Base.$f(A::QuantityArray) = $f(ustrip(A))
end

Base.getindex(A::QuantityArray, i...) = quantity_type(A)(getindex(ustrip(A), i...), dimension(A))
Base.setindex!(A::QuantityArray{T,N,D,Q}, v::Q, i...) where {T,N,D,Q<:AbstractQuantity} = dimension(A) == dimension(v) ? unsafe_setindex!(A, v, i...) : throw(DimensionError(A, v))
Base.setindex!(A::QuantityArray{T,N,D,Q}, v::AbstractQuantity, i...) where {T,N,D,Q<:AbstractQuantity} = error("Cannot set values in a quantity array with element type $(Q) with different element type: $(typeof(v)).")
# TODO: Should this dimension check be removed?
# TODO: This does not allow for efficient broadcasting; as the dimension calculation is repeated...

unsafe_setindex!(A, v, i...) = setindex!(ustrip(A), ustrip(v), i...)

Base.IndexStyle(::Type{Q}) where {Q<:QuantityArray} = IndexStyle(array_type(Q))

Base.similar(A::QuantityArray) = QuantityArray(similar(ustrip(A)), dimension(A))
Base.similar(A::QuantityArray, ::Type{S}) where {S} = QuantityArray(similar(ustrip(A), S), dimension(A))
Base.similar(A::QuantityArray, dims::Dims) = QuantityArray(similar(ustrip(A), dims), dimension(A))
Base.similar(A::QuantityArray, ::Type{S}, dims::Dims) where {S} = QuantityArray(similar(ustrip(A), S, dims), dimension(A))

Base.similar(::Type{QA}) where {T,QA<:QuantityArray{T}} = QuantityArray(similar(array_type(QA)), dim_type(QA)())
Base.similar(::Type{QA}, ::Type{S}) where {T,QA<:QuantityArray{T},S} = QuantityArray(similar(array_type(QA), S), dim_type(QA)())
Base.similar(::Type{QA}, dims::Dims) where {T,QA<:QuantityArray{T}} = QuantityArray(similar(array_type(QA), dims), dim_type(QA)())
Base.similar(::Type{QA}, ::Type{S}, dims::Dims) where {T,QA<:QuantityArray{T},S} = QuantityArray(similar(array_type(QA), S, dims), dim_type(QA)())

function Base.BroadcastStyle(::Type{QA}) where {QA<:QuantityArray}
    return Broadcast.ArrayStyle{QA}()
end
function Base.BroadcastStyle(
    ::Broadcast.ArrayStyle{QA1}, ::Broadcast.ArrayStyle{QA2}
) where {
    T1,T2,N,V1<:AbstractArray{T1,N},V2<:AbstractArray{T2,N},D<:AbstractDimensions,
    Q1<:AbstractQuantity{T1,D},Q2<:AbstractQuantity{T2,D},
    QA1<:QuantityArray{T1,N,D,Q1,V1},QA2<:QuantityArray{T2,N,D,Q2,V2}
}
    T = promote_type(T1,T2)
    V = promote_type(V1,V2)
    Q = constructor_of(Q1){T,D}
    return Broadcast.ArrayStyle{QuantityArray{T,N,D,Q,V}}()
end
function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QA}}, ::Type{ElType}) where {QA<:QuantityArray,ElType}
    q = find_q(bc)::quantity_type(QA)
    return QuantityArray(similar(array_type(QA), axes(bc)), dimension(q))
end
# https://discourse.julialang.org/t/defining-broadcast-for-custom-types-the-example-in-the-docs-fails/32291/2
find_q(x::Base.Broadcast.Extruded) = x.x
find_q(bc::Base.Broadcast.Broadcasted) = 
    let ar=ntuple(i -> find_q(bc.args[i]), Val(length(bc.args)))
        bc.f(ar...)
    end

find_q(args::Tuple) = find_q(find_q(first(args)), Base.tail(args))
find_q(x) = (@show x; x)
find_q(::Tuple{}) = error("Unexpected.")
find_q(q::AbstractQuantity) = q
find_q(q::AbstractQuantity, ::Any) = q
find_q(q::QuantityArray) = first(q)
find_q(q::QuantityArray, ::Any) = first(q)
find_q(::Any, rest) = find_q(rest)

_print_array_type(io::IO, ::Type{QA}) where {QA<:QuantityArray} = print(io, "QuantityArray(::", array_type(QA), ", ::", quantity_type(QA), ")")
Base.showarg(io::IO, v::QuantityArray, _) = _print_array_type(io, typeof(v))
Base.show(io::IO, ::MIME"text/plain", ::Type{QA}) where {QA<:QuantityArray} = _print_array_type(io, QA)
