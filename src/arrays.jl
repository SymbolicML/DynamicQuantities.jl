"""
    QuantityArray{T,N,D<:AbstractDimensions,Q<:AbstractQuantity,V<:AbstractArray}

An array of quantities with value `value` of type `V` and dimensions `dimensions` of type `D`
(which are shared across all elements of the array). This is a subtype of `AbstractArray{Q,N}`,
and so can be used in most places where a normal array would be used.

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

    function QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity,_V<:AbstractArray{_T,_N}}
        Q_out = constructor_of(_Q){_T,_D}
        return new{_T,_N,_D,Q_out,_V}(v, d)
    end
end

# Construct with a Quantity (easier, as you can use the units):
QuantityArray(v::AbstractArray; kws...) = QuantityArray(v, DEFAULT_DIM_TYPE(; kws...))
QuantityArray(v::AbstractArray, d::AbstractDimensions) = QuantityArray(v, d, Quantity)
QuantityArray(v::AbstractArray, q::AbstractQuantity) = QuantityArray(v .* ustrip(q), dimension(q), typeof(q))
QuantityArray(v::QA) where {Q<:AbstractQuantity,QA<:AbstractArray{Q}} = allequal(dimension.(v)) ? QuantityArray(ustrip.(v), dimension(first(v)), Q) : throw(DimensionError(first(v), v))
# TODO: Should this check that the dimensions are the same?

function Base.promote_rule(::Type{QA1}, ::Type{QA2}) where {QA1<:QuantityArray,QA2<:QuantityArray}
    D = promote_type(dim_type.((QA1, QA2))...)
    Q = promote_type(quantity_type.((QA1, QA2))...)
    T = promote_type(value_type.((QA1, QA2))...)
    V = promote_type(array_type.((QA1, QA2))...)
    N = ndims(QA1)

    @assert(Q <: AbstractQuantity{T,D}, "Incompatible promotion rules.")
    @assert(V <: AbstractArray{T}, "Incompatible promotion rules.")

    if N != ndims(QA2)
        return QuantityArray{T,_N,D,Q,V} where {_N}
    else
        return QuantityArray{T,N,D,Q,V}
    end
end

@inline ustrip(A::QuantityArray) = A.value
@inline dimension(A::QuantityArray) = A.dimensions

array_type(::Type{A}) where {T,A<:QuantityArray{T}} = Array{T,1}
array_type(::Type{A}) where {T,N,A<:QuantityArray{T,N}} = Array{T,N}
array_type(::Type{A}) where {T,N,D,Q,V,A<:QuantityArray{T,N,D,Q,V}} = V
array_type(A) = array_type(typeof(A))

quantity_type(::Type{A}) where {T,N,D,Q,A<:QuantityArray{T,N,D,Q}} = Q
quantity_type(A) = quantity_type(typeof(A))

dim_type(::Type{A}) where {A<:QuantityArray} = DEFAULT_DIM_TYPE
dim_type(::Type{A}) where {T,N,D,A<:QuantityArray{T,N,D}} = D
dim_type(A) = dim_type(typeof(A))

value_type(::Type{A}) where {A<:QuantityArray} = DEFAULT_VALUE_TYPE
value_type(::Type{A}) where {T,A<:QuantityArray{T}} = T
value_type(::Type{Q}) where {T,Q<:AbstractQuantity{T}} = T
value_type(A) = value_type(typeof(A))

# One field:
for f in (:size, :length, :axes)
    @eval Base.$f(A::QuantityArray) = $f(ustrip(A))
end

function Base.getindex(A::QuantityArray, i...)
    output_value = getindex(ustrip(A), i...)
    if isa(output_value, AbstractArray)
        return QuantityArray(output_value, dimension(A), quantity_type(A))
    else
        return new_quantity(quantity_type(A), output_value, dimension(A))
    end
end
Base.setindex!(A::QuantityArray{T,N,D,Q}, v::Q, i...) where {T,N,D,Q<:AbstractQuantity} = dimension(A) == dimension(v) ? unsafe_setindex!(A, v, i...) : throw(DimensionError(A, v))
Base.setindex!(A::QuantityArray{T,N,D,Q}, v::AbstractQuantity, i...) where {T,N,D,Q<:AbstractQuantity} = setindex!(A, convert(Q, v), i...)

unsafe_setindex!(A, v, i...) = setindex!(ustrip(A), ustrip(v), i...)

Base.IndexStyle(::Type{Q}) where {Q<:QuantityArray} = IndexStyle(array_type(Q))

Base.similar(A::QuantityArray) = QuantityArray(similar(ustrip(A)), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}) where {S} = QuantityArray(similar(ustrip(A), S), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, dims::Dims) = QuantityArray(similar(ustrip(A), dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}, dims::Dims) where {S} = QuantityArray(similar(ustrip(A), S, dims), dimension(A), quantity_type(A))

Base.similar(::Type{QA}) where {T,QA<:QuantityArray{T}} = QuantityArray(similar(array_type(QA)), dim_type(QA)(), quantity_type(QA))
Base.similar(::Type{QA}, ::Type{S}) where {T,QA<:QuantityArray{T},S} = QuantityArray(similar(array_type(QA), S), dim_type(QA)(), quantity_type(QA))
Base.similar(::Type{QA}, dims::Dims) where {T,QA<:QuantityArray{T}} = QuantityArray(similar(array_type(QA), dims), dim_type(QA)(), quantity_type(QA))
Base.similar(::Type{QA}, ::Type{S}, dims::Dims) where {T,QA<:QuantityArray{T},S} = QuantityArray(similar(array_type(QA), S, dims), dim_type(QA)(), quantity_type(QA))

function Base.BroadcastStyle(::Type{QA}) where {QA<:QuantityArray}
    return Broadcast.ArrayStyle{QA}()
end
function Base.BroadcastStyle(::Broadcast.ArrayStyle{QA1}, ::Broadcast.ArrayStyle{QA2}) where {QA1<:QuantityArray,QA2<:QuantityArray}
    return Broadcast.ArrayStyle{promote_type(QA1, QA2)}()
end
function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QA}}, ::Type{ElType}) where {QA<:QuantityArray,ElType}
    output_array_type = constructor_of(array_type(QA)){unwrap_quantity(ElType)}
    output_array = similar(output_array_type, axes(bc))

    if ElType <: AbstractQuantity
        first_output = materialize_first(bc)
        if typeof(first_output) != ElType
            @warn (
                "Materialization of first element likely failed. "
                * "Please submit a bug report with information on "
                * "the function you are broadcasting."
            )
        end
        return QuantityArray(output_array, dimension(first_output), ElType)
    else
        return output_array
    end
end
unwrap_quantity(::Type{Q}) where {T,Q<:AbstractQuantity{T}} = T
unwrap_quantity(::Type{T}) where {T} = T

# Basically, we want to solve a single element to find the output dimension.
# Then we can put results in the output `QuantityArray`.
materialize_first(bc::Base.Broadcast.Broadcasted) = bc.f(materialize_first.(bc.args)...)

# Base cases
materialize_first(q::AbstractQuantity) = q
materialize_first(q::AbstractQuantity, ::Any) = q
materialize_first(q::QuantityArray) = first(q)
materialize_first(q::QuantityArray, ::Any) = first(q)
materialize_first(q::AbstractArray{Q}) where {Q<:AbstractQuantity} = first(q)
materialize_first(q::AbstractArray{Q}, ::Any) where {Q<:AbstractQuantity} = first(q)

# Derived calls
materialize_first(r::Base.RefValue) = materialize_first(r.x)
materialize_first(x::Base.Broadcast.Extruded) = materialize_first(x.x)
materialize_first(args::Tuple) = materialize_first(first(args), Base.tail(args))
materialize_first(args::AbstractArray) = length(args) >= 1 ? materialize_first(args[begin], args[begin+1:end]) : error("Unexpected broadcast format. Please submit a bug report.")
materialize_first(::Tuple{}) = error("Unexpected broadcast format. Please submit a bug report.")
materialize_first(::Any, rest) = materialize_first(rest)

# Everything else:
materialize_first(x) = x

function _print_array_type(io::IO, ::Type{QA}) where {QA<:QuantityArray}
    return print(io, "QuantityArray(::", array_type(QA), ", ::", quantity_type(QA), ")")
end
Base.showarg(io::IO, v::QuantityArray, _) = _print_array_type(io, typeof(v))
Base.show(io::IO, ::MIME"text/plain", ::Type{QA}) where {QA<:QuantityArray} = _print_array_type(io, QA)

# Other array operations:
Base.copy(A::QuantityArray) = QuantityArray(copy(ustrip(A)), copy(dimension(A)), quantity_type(A))
function Base.cat(A::QuantityArray...; dims)
    allequal(dimension.(A)) || throw(DimensionError(A[begin], A[begin+1:end]))
    return QuantityArray(cat(ustrip.(A)...; dims=dims), dimension(A[begin]), quantity_type(A[begin]))
end
Base.hcat(A::QuantityArray...) = cat(A...; dims=2)
Base.vcat(A::QuantityArray...) = cat(A...; dims=1)
Base.fill(x::AbstractQuantity, dims::Dims...) = QuantityArray(fill(ustrip(x), dims...), dimension(x), typeof(x))

ulength(q::QuantityArray) = ulength(dimension(q))
umass(q::QuantityArray) = umass(dimension(q))
utime(q::QuantityArray) = utime(dimension(q))
ucurrent(q::QuantityArray) = ucurrent(dimension(q))
utemperature(q::QuantityArray) = utemperature(dimension(q))
uluminosity(q::QuantityArray) = uluminosity(dimension(q))
uamount(q::QuantityArray) = uamount(dimension(q))
