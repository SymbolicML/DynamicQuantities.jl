import Compat: allequal

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
QuantityArray(v::QA) where {Q<:AbstractQuantity,QA<:AbstractArray{Q}} =
    let
        allequal(dimension.(v)) || throw(DimensionError(first(v), v))
        QuantityArray(ustrip.(v), dimension(first(v)), Q)
    end

function Base.promote_rule(::Type{QA1}, ::Type{QA2}) where {QA1<:QuantityArray,QA2<:QuantityArray}
    D = promote_type(dim_type.((QA1, QA2))...)
    Q = promote_type(quantity_type.((QA1, QA2))...)
    T = promote_type(value_type.((QA1, QA2))...)
    V = promote_type(array_type.((QA1, QA2))...)
    N = ndims(QA1)

    @assert(
        N == ndims(QA2),
        "Cannot promote quantity arrays with different dimensions."
    )
    @assert(
        Q <: AbstractQuantity{T,D} && V <: AbstractArray{T},
        "Incompatible promotion rules between\n    $(QA1)\nand\n    $(QA2)\nPlease convert to a common quantity type first."
    )

    return QuantityArray{T,N,D,Q,V}
end

function Base.convert(::Type{QA}, A::QA) where {QA<:QuantityArray}
    return A
end
function Base.convert(::Type{QA1}, A::QA2) where {QA1<:QuantityArray,QA2<:QuantityArray}
    Q = quantity_type(QA1)
    V = array_type(QA1)
    N = ndims(QA1)

    raw_array = Base.Fix1(convert, Q).(A)
    output = QuantityArray(convert(constructor_of(V){Q,N}, raw_array))
    # TODO: This will mess with static arrays

    return output::QA1
end

@inline ustrip(A::QuantityArray) = A.value
@inline dimension(A::QuantityArray) = A.dimensions

array_type(::Type{<:QuantityArray{T,N,D,Q,V}}) where {T,N,D,Q,V} = V
array_type(A::QuantityArray) = array_type(typeof(A))

quantity_type(::Type{<:QuantityArray{T,N,D,Q}}) where {T,N,D,Q} = Q
quantity_type(A::QuantityArray) = quantity_type(typeof(A))

dim_type(::Type{<:QuantityArray{T,N,D}}) where {T,N,D} = D
dim_type(A::QuantityArray) = dim_type(typeof(A))

value_type(::Type{<:AbstractQuantity{T}}) where {T} = T
value_type(::Type{<:QuantityArray{T}}) where {T} = T
value_type(A::Union{<:QuantityArray,<:AbstractQuantity}) = value_type(typeof(A))

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
function Base.setindex!(A::QuantityArray{T,N,D,Q}, v::Q, i...) where {T,N,D,Q<:AbstractQuantity}
    dimension(A) == dimension(v) || throw(DimensionError(A, v))
    return unsafe_setindex!(A, v, i...)
end
function Base.setindex!(A::QuantityArray{T,N,D,Q}, v::AbstractQuantity, i...) where {T,N,D,Q<:AbstractQuantity}
    return setindex!(A, convert(Q, v), i...)
end

unsafe_setindex!(A, v, i...) = setindex!(ustrip(A), ustrip(v), i...)

Base.IndexStyle(::Type{Q}) where {Q<:QuantityArray} = IndexStyle(array_type(Q))

const IntOrOneTo = Union{Integer, Base.OneTo}

# Unfortunately this mess of `similar` is required to avoid ambiguous methods.
# c.f. base/abstractarray.jl
Base.similar(A::QuantityArray) = QuantityArray(similar(ustrip(A)), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}) where {S} = QuantityArray(similar(ustrip(A), S), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, dims::Dims{N}) where {N} = QuantityArray(similar(ustrip(A), dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, dims::Tuple{IntOrOneTo, Vararg{IntOrOneTo}}) = QuantityArray(similar(ustrip(A), dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, dims::Tuple{Integer, Vararg{Integer}}) = QuantityArray(similar(ustrip(A), dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}, dims::Dims{N}) where {S,N} = QuantityArray(similar(ustrip(A), S, dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}, dims::Tuple{IntOrOneTo, Vararg{IntOrOneTo}}) where {S} = QuantityArray(similar(ustrip(A), S, dims), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}, dims::Tuple{Integer, Vararg{Integer}}) where {S} = QuantityArray(similar(ustrip(A), S, dims), dimension(A), quantity_type(A))

function Base.BroadcastStyle(::Type{QA}) where {QA<:QuantityArray}
    return Broadcast.ArrayStyle{QA}()
end
function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QA}}, ::Type{ElType}) where {QA<:QuantityArray,ElType<:AbstractQuantity}
    T = value_type(ElType)
    output_array = similar(bc, T)
    first_output::ElType = materialize_first(bc)
    return QuantityArray(output_array, dimension(first_output)::dim_type(ElType), ElType)
end
function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QuantityArray{T,N,D,Q,V}}}, ::Type{ElType}) where {T,N,D,Q,V<:Array{T,N},ElType}
    return similar(Array{ElType}, axes(bc))
end
function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QuantityArray{T,N,D,Q,V}}}, ::Type{ElType}) where {T,N,D,Q,V,ElType}
    # To deal with things like StaticArrays, we need to rely on
    # only `similar(::Type{ArrayType}, axes)`. We can't specify the
    # element type in `similar` if we only give the array type.
    # TODO: However, this results in a redundant allocation.
    return (_ -> zero(ElType)).(similar(V, axes(bc)))
end

# Basically, we want to solve a single element to find the output dimension.
# Then we can put results in the output `QuantityArray`.
materialize_first(bc::Base.Broadcast.Broadcasted) = bc.f(materialize_first.(bc.args)...)

# Base cases
materialize_first(q::AbstractQuantity{<:AbstractArray}) = new_quantity(typeof(q), first(ustrip(q)), dimension(q))
materialize_first(q::AbstractQuantity) = q
materialize_first(q::QuantityArray) = first(q)
materialize_first(q::AbstractArray{Q}) where {Q<:AbstractQuantity} = first(q)

# Derived calls
materialize_first(r::Base.RefValue) = materialize_first(r.x)
materialize_first(x::Base.Broadcast.Extruded) = materialize_first(x.x)
materialize_first(args::Tuple) = materialize_first(first(args))
materialize_first(args::AbstractArray) =
    let
        length(args) >= 1 || error("Unexpected broadcast format. Please submit a bug report.")
        materialize_first(args[begin])
    end
materialize_first(::Tuple{}) = error("Unexpected broadcast format. Please submit a bug report.")

# Everything else:
materialize_first(x) = x

function _print_array_type(io::IO, ::Type{QA}) where {QA<:QuantityArray}
    return print(io, "QuantityArray(::", array_type(QA), ", ::", quantity_type(QA), ")")
end
Base.showarg(io::IO, v::QuantityArray, _) = _print_array_type(io, typeof(v))
Base.show(io::IO, ::MIME"text/plain", ::Type{QA}) where {QA<:QuantityArray} = _print_array_type(io, QA)

# Other array operations:
Base.copy(A::QuantityArray) = QuantityArray(copy(ustrip(A)), copy(dimension(A)), quantity_type(A))
for f in (:cat, :hcat, :vcat)
    preamble = quote
        allequal(dimension.(A)) || throw(DimensionError(A[begin], A[begin+1:end]))
        A = promote(A...)
        dimensions = dimension(A[begin])
        Q = quantity_type(A[begin])
    end
    if f == :cat
        @eval function Base.$f(A::QuantityArray...; dims)
            $preamble
            return QuantityArray($f(ustrip.(A)...; dims), dimensions, Q)
        end
    else
        @eval function Base.$f(A::QuantityArray...)
            $preamble
            return QuantityArray($f(ustrip.(A)...), dimensions, Q)
        end
    end
end
Base.fill(x::AbstractQuantity, dims::Dims...) = QuantityArray(fill(ustrip(x), dims...), dimension(x), typeof(x))
Base.fill(x::AbstractQuantity, t::Tuple{}) = QuantityArray(fill(ustrip(x), t), dimension(x), typeof(x))

ulength(q::QuantityArray) = ulength(dimension(q))
umass(q::QuantityArray) = umass(dimension(q))
utime(q::QuantityArray) = utime(dimension(q))
ucurrent(q::QuantityArray) = ucurrent(dimension(q))
utemperature(q::QuantityArray) = utemperature(dimension(q))
uluminosity(q::QuantityArray) = uluminosity(dimension(q))
uamount(q::QuantityArray) = uamount(dimension(q))
