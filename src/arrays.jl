import Compat: allequal

"""
    QuantityArray{T,N,D<:AbstractDimensions,Q<:UnionAbstractQuantity,V<:AbstractArray}

An array of quantities with value `value` of type `V` and dimensions `dimensions` of type `D`
(which are shared across all elements of the array). This is a subtype of `AbstractArray{Q,N}`,
and so can be used in most places where a normal array would be used, including broadcasting operations.

# Fields

- `value`: The underlying array of values. Access with `ustrip(a)`.
- `dimensions`: The dimensions of the array. Access with `dimension(a)`.

# Constructors

- `QuantityArray(v::AbstractArray, d::AbstractDimensions)`: Create a `QuantityArray` with value `v` and dimensions `d`,
  using `Quantity` if the eltype of `v` is numeric, and `GenericQuantity` otherwise.
- `QuantityArray(v::AbstractArray{<:Number}, q::AbstractQuantity)`: Create a `QuantityArray` with value `v` and dimensions inferred
   with `dimension(q)`. This is so that you can easily create an array with the units module, like so:
   ```julia
   julia> A = QuantityArray(randn(32), 1u"m")
   ```
- `QuantityArray(v::AbstractArray{<:Any}, q::AbstractGenericQuantity)`: Create a `QuantityArray` with
    value `v` and dimensions inferred with `dimension(q)`.
    This is so that you can easily create quantity arrays of non-numeric eltypes, like so:
   ```julia
   julia> A = QuantityArray([[1.0], [2.0, 3.0]], GenericQuantity(1u"m"))
   ```
- `QuantityArray(v::AbstractArray{<:UnionAbstractQuantity})`: Create a `QuantityArray` from an array of quantities. This means the following
  syntax works:
  ```julia
  julia> A = QuantityArray(randn(32) .* 1u"km/s")
  ```
- `QuantityArray(v::AbstractArray; kws...)`: Create a `QuantityArray` with dimensions inferred from the keyword arguments. For example:
  ```julia
  julia> A = QuantityArray(randn(32); length=1)
  ```
  is equivalent to
  ```julia
  julia> A = QuantityArray(randn(32), u"m")
  ```
  The keyword arguments are passed to `DEFAULT_DIM_TYPE`.
"""
struct QuantityArray{T,N,D<:AbstractDimensions,Q<:UnionAbstractQuantity{T,D},V<:AbstractArray{T,N}} <: AbstractArray{Q,N}
    value::V
    dimensions::D

    function QuantityArray(v::_V, d::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:UnionAbstractQuantity,_V<:AbstractArray{_T,_N}}
        Q_out = with_type_parameters(_Q, _T, _D)
        return new{_T,_N,_D,Q_out,_V}(v, d)
    end
end

QuantityArray(v::AbstractArray; kws...) = QuantityArray(v, DEFAULT_DIM_TYPE(; kws...))
for (type, base_type, default_type) in ABSTRACT_QUANTITY_TYPES
    @eval QuantityArray(v::AbstractArray{<:$base_type}, q::$type) = QuantityArray(v .* ustrip(q), dimension(q), typeof(q))

    # Only define defaults for Quantity and GenericQuantity. Other types, the user needs to declare explicitly.
    if type in (AbstractQuantity, AbstractGenericQuantity)
        @eval QuantityArray(v::AbstractArray{<:$base_type}, d::AbstractDimensions) = QuantityArray(v, d, $default_type)
    end
end
QuantityArray(v::QA) where {Q<:UnionAbstractQuantity,QA<:AbstractArray{Q}} =
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
        Q <: UnionAbstractQuantity{T,D} && V <: AbstractArray{T},
        "Incompatible promotion rules between\n    $(QA1)\nand\n    $(QA2)\nPlease convert to a common quantity type first."
    )

    return QuantityArray{T,N,D,Q,V}
end

function Base.convert(::Type{QA}, A::QA) where {QA<:QuantityArray}
    return A
end
function Base.convert(::Type{QA1}, A::QA2) where {QA1<:QuantityArray,QA2<:QuantityArray}
    Q1 = quantity_type(QA1)
    Q2 = quantity_type(QA2)
    T = value_type(QA1)
    V = array_type(QA1)

    return QuantityArray(
        convert(V, ustrip(A)),
        convert(Q1, new_quantity(Q2, one(T), dimension(A))),
    )::QA1
end

@inline ustrip(A::QuantityArray) = A.value
@inline dimension(A::QuantityArray) = A.dimensions

array_type(::Type{<:QuantityArray{T,N,D,Q,V}}) where {T,N,D,Q,V} = V
array_type(A::QuantityArray) = array_type(typeof(A))

quantity_type(::Type{<:QuantityArray{T,N,D,Q}}) where {T,N,D,Q} = Q
quantity_type(A::QuantityArray) = quantity_type(typeof(A))

dim_type(::Type{<:QuantityArray{T,N,D}}) where {T,N,D} = D
dim_type(A::QuantityArray) = dim_type(typeof(A))

value_type(::Type{<:UnionAbstractQuantity{T}}) where {T} = T
value_type(::Type{<:QuantityArray{T}}) where {T} = T
value_type(A::Union{<:QuantityArray,<:UnionAbstractQuantity}) = value_type(typeof(A))

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
function Base.setindex!(A::QuantityArray{T,N,D,Q}, v::Q, i...) where {T,N,D,Q<:UnionAbstractQuantity}
    dimension(A) == dimension(v) || throw(DimensionError(A, v))
    return unsafe_setindex!(A, v, i...)
end
function Base.setindex!(A::QuantityArray{T,N,D,Q}, v::UnionAbstractQuantity, i...) where {T,N,D,Q<:UnionAbstractQuantity}
    return setindex!(A, convert(Q, v)::Q, i...)
end

unsafe_setindex!(A, v, i...) = setindex!(ustrip(A), ustrip(v), i...)

Base.IndexStyle(::Type{Q}) where {Q<:QuantityArray} = IndexStyle(array_type(Q))

# Methods which return a single element
for f in (:pop!, :popfirst!, :popat!)
    args = f == :popat! ? (:(i::Integer),) : ()
    @eval function Base.$(f)(A::QuantityArray, $(args...))
        new_quantity(quantity_type(A), $(f)(ustrip(A), $(args...)), dimension(A))
    end
end
# Methods which return the array
for f in (:resize!, :sizehint!, :deleteat!, :empty!)
    args =
        if f in (:resize!, :sizehint!)
            (:(n::Integer),)
        elseif f == :deleteat!
            (:(i),)
        else  # f == :empty!
            ()
        end
    @eval function Base.$(f)(A::QuantityArray, $(args...))
        $(f)(ustrip(A), $(args...))
        A
    end
end
# Methods which return the array, and also check the dimension
for f in (:push!, :pushfirst!, :insert!)
    args = f == :insert! ? (:(i::Integer),) : ()
    @eval function Base.$(f)(A::QuantityArray, $(args...), v::UnionAbstractQuantity...)
        v = (vi -> convert(quantity_type(A), vi)).(v)
        all(vi -> dimension(A) == dimension(vi), v) || throw(DimensionError(A, v))
        $(f)(ustrip(A), $(args...), map(ustrip, v)...)
        A
    end
    # TODO: Note that `insert!` is technically the wrong signature (though it will throw
    #   an error in the called method).
end
# Methods which combine arrays
for f in (:append!, :prepend!)
    @eval begin
        function Base.$(f)(A::QuantityArray, B::QuantityArray)
            B2 = convert(typeof(A), B)
            dimension(A) == dimension(B2) || throw(DimensionError(A, B))
            $(f)(ustrip(A), ustrip(B2))
            A
        end
        function Base.$(f)(A::QuantityArray, B::Vector{<:UnionAbstractQuantity})
            B = (bi -> convert(quantity_type(A), bi)).(B)
            dimension(A) == dimension(B) || throw(DimensionError(A, B))
            $(f)(ustrip(A), ustrip.(B))
            A
        end
    end
end

Base.similar(A::QuantityArray) = QuantityArray(similar(ustrip(A)), dimension(A), quantity_type(A))
Base.similar(A::QuantityArray, ::Type{S}) where {S} = QuantityArray(similar(ustrip(A), S), dimension(A), quantity_type(A))
for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval Base.similar(A::QuantityArray, ::Type{S}) where {S<:$type} = QuantityArray(similar(ustrip(A), value_type(S)), dimension(A), S)
end

# Unfortunately this mess of `similar` is required to avoid ambiguous methods.
# c.f. base/abstractarray.jl
for dim_type in (:(Dims), :(Tuple{Union{Integer,Base.OneTo},Vararg{Union{Integer,Base.OneTo}}}), :(Tuple{Integer, Vararg{Integer}}))
    @eval begin
        Base.similar(A::QuantityArray, dims::$dim_type) = QuantityArray(similar(ustrip(A), dims), dimension(A), quantity_type(A))
        Base.similar(A::QuantityArray, ::Type{S}, dims::$dim_type) where {S} = QuantityArray(similar(ustrip(A), S, dims), dimension(A), quantity_type(A))
    end
    for (type, _, _) in ABSTRACT_QUANTITY_TYPES
        @eval Base.similar(A::QuantityArray, ::Type{S}, dims::$dim_type) where {S<:$type} = QuantityArray(similar(ustrip(A), value_type(S), dims), dimension(A), S)
    end
end

# `_similar_for` in Base does not account for changed dimensions, so
# we need to overload it for QuantityArray.
Base._similar_for(c::QuantityArray, ::Type{T}, itr, ::Base.HasShape, axs) where {T<:UnionAbstractQuantity} =
    QuantityArray(similar(ustrip(c), value_type(T), axs), dimension(materialize_first(itr))::dim_type(T), T)
Base._similar_for(c::QuantityArray, ::Type{T}, itr, ::Base.HasShape, axs) where {T} =
    similar(ustrip(c), T, axs)

# These methods are not yet implemented, but the default implementation is dangerous,
# as it may cause a stack overflow, so we raise a more helpful error instead.
Base._similar_for(::QuantityArray, ::Type{T}, _, ::Base.SizeUnknown, ::Nothing) where {T} =
    error("Not implemented. Please raise an issue on DynamicQuantities.jl.")
Base._similar_for(::QuantityArray, ::Type{T}, _, ::Base.HasLength, ::Integer) where {T} =
    error("Not implemented. Please raise an issue on DynamicQuantities.jl.")

# In earlier Julia, `Base._similar_for` has different signatures.
@static if hasmethod(Base._similar_for, Tuple{Array,Type,Any,Base.HasShape})
    @eval Base._similar_for(c::QuantityArray, ::Type{T}, itr, ::Base.HasShape) where {T<:UnionAbstractQuantity} =
        QuantityArray(similar(ustrip(c), value_type(T), axes(itr)), dimension(materialize_first(itr))::dim_type(T), T)
    @eval Base._similar_for(c::QuantityArray, ::Type{T}, itr, ::Base.HasShape) where {T} =
        similar(ustrip(c), T, axes(itr))
end
@static if hasmethod(Base._similar_for, Tuple{Array,Type,Any,Base.HasLength})
    @eval Base._similar_for(::QuantityArray, ::Type{T}, _, ::Base.HasLength) where {T} =
        error("Not implemented. Please raise an issue on DynamicQuantities.jl.")
end
@static if hasmethod(Base._similar_for, Tuple{Array,Type,Any,Base.SizeUnknown})
    @eval Base._similar_for(::QuantityArray, ::Type{T}, _, ::Base.SizeUnknown) where {T} =
        error("Not implemented. Please raise an issue on DynamicQuantities.jl.")
end
@static if hasmethod(Base._similar_for, Tuple{Array,Type,Any,Any})
    @eval Base._similar_for(::QuantityArray, ::Type{T}, _, _) where {T} =
        error("Not implemented. Please raise an issue on DynamicQuantities.jl.")
end

Base.BroadcastStyle(::Type{QA}) where {QA<:QuantityArray} = Broadcast.ArrayStyle{QA}()

function Base.similar(bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{QA}}, ::Type{ElType}) where {QA<:QuantityArray,ElType<:UnionAbstractQuantity}
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
materialize_first(q::AbstractGenericQuantity{<:AbstractArray}) = new_quantity(typeof(q), first(ustrip(q)), dimension(q))
materialize_first(q::UnionAbstractQuantity) = q
materialize_first(q::QuantityArray) = first(q)
materialize_first(q::AbstractArray{Q}) where {Q<:UnionAbstractQuantity} = first(q)

# Derived calls
materialize_first(g::Base.Generator) = materialize_first(first(g))
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
Base.fill(x::UnionAbstractQuantity, dims::Dims...) = QuantityArray(fill(ustrip(x), dims...), dimension(x), typeof(x))
Base.fill(x::UnionAbstractQuantity, t::Tuple{}) = QuantityArray(fill(ustrip(x), t), dimension(x), typeof(x))

# Will be overloaded by `DynamicQuantitiesLinearAlgebraExt`:
_norm(_) = error("Please load the `LinearAlgebra.jl` package.")

# Define isapprox for vectors of Quantity's
struct AutoATol end
get_atol(el, ::AutoATol) = zero(el)
get_atol(_, atol) = atol

function Base.isapprox(
    u::AbstractArray{<:UnionAbstractQuantity},
    v::AbstractArray{<:UnionAbstractQuantity};
    atol=AutoATol(),
    rtol=Base.rtoldefault(promote_type(value_type(eltype(u)), value_type(eltype(v)))),
    nans::Bool=false,
    norm::F=_norm
) where {F<:Function}
    if all(i -> dimension(u[i]) == dimension(v[i]), eachindex(u, v))
        d = norm(u .- v)
        if isfinite(d)
            return d <= max(get_atol(first(u), atol), rtol*max(norm(u), norm(v)))
        end
    end
    # Fall back to a component-wise approximate comparison
    return all(
        i -> isapprox(u[i], v[i]; rtol=rtol, atol=get_atol(u[i], atol), nans=nans),
        eachindex(u, v)
    )
end

# Define isapprox for QuantityArray's
function Base.isapprox(
    u::QuantityArray,
    v::QuantityArray;
    atol=AutoATol(),
    rtol=Base.rtoldefault(promote_type(value_type(u), value_type(v))),
    nans::Bool=false,
    norm::F=_norm
) where {F<:Function}
    d = norm(u .- v)
    _atol = get_atol(first(u), atol)
    if isfinite(d)
        return d <= max(_atol, rtol*max(norm(u), norm(v)))
    end
    # Fall back to a component-wise approximate comparison
    return all(
        i -> isapprox(u[i], v[i]; rtol=rtol, atol=_atol, nans=nans),
        eachindex(u, v)
    )
end

# Unit functions
ulength(q::QuantityArray) = ulength(dimension(q))
umass(q::QuantityArray) = umass(dimension(q))
utime(q::QuantityArray) = utime(dimension(q))
ucurrent(q::QuantityArray) = ucurrent(dimension(q))
utemperature(q::QuantityArray) = utemperature(dimension(q))
uluminosity(q::QuantityArray) = uluminosity(dimension(q))
uamount(q::QuantityArray) = uamount(dimension(q))
