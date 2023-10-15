import Tricks: static_fieldnames
import Compat: allequal

function map_dimensions(f::F, args::AbstractDimensions...) where {F<:Function}
    dimension_type = promote_type(typeof(args).parameters...)
    dimension_names = static_fieldnames(dimension_type)
    return new_dimensions(
        dimension_type,
        (
            f((getproperty(arg, dim) for arg in args)...)
            for dim in dimension_names
        )...
    )
end
@generated function all_dimensions(f::F, args::AbstractDimensions...) where {F<:Function}
    # Test a function over all dimensions
    output = Expr(:&&)
    dimension_type = promote_type(args...)
    for dim in Base.fieldnames(dimension_type)
        f_expr = :(f())
        for i=1:length(args)
            push!(f_expr.args, :(args[$i].$dim))
        end
        push!(output.args, f_expr)
    end
    return output
end

Base.float(q::AbstractUnionQuantity) = new_quantity(typeof(q), float(ustrip(q)), dimension(q))
Base.convert(::Type{T}, q::AbstractUnionQuantity) where {T<:Number} =
    let
        @assert iszero(dimension(q)) "$(typeof(q)): $(q) has dimensions! Use `ustrip` instead."
        return convert(T, ustrip(q))
    end
function Base.promote_rule(::Type{Dimensions{R1}}, ::Type{Dimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{<:GenericQuantity{T1,D1}}, ::Type{<:GenericQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:Quantity{T1,D1}}, ::Type{<:GenericQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:Quantity{T1,D1}}, ::Type{<:Quantity{T2,D2}}) where {T1,T2,D1,D2}
    return Quantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{T1}, ::Type{<:Quantity{T2,D2}}) where {T1<:Number,T2,D2}
    return Quantity{promote_type(T1,T2),D2}
end

Base.keys(d::AbstractDimensions) = static_fieldnames(typeof(d))
Base.getindex(d::AbstractDimensions, k::Symbol) = getfield(d, k)

# Compatibility with `.*`
Base.size(q::AbstractUnionQuantity) = size(ustrip(q))
Base.length(q::AbstractUnionQuantity) = length(ustrip(q))
Base.axes(q::AbstractUnionQuantity) = axes(ustrip(q))
Base.iterate(qd::AbstractUnionQuantity, maybe_state...) =
    let subiterate=iterate(ustrip(qd), maybe_state...)
        subiterate === nothing && return nothing
        return new_quantity(typeof(qd), subiterate[1], dimension(qd)), subiterate[2]
    end
Base.ndims(::Type{<:AbstractUnionQuantity{T}}) where {T} = ndims(T)
Base.ndims(q::AbstractUnionQuantity) = ndims(ustrip(q))
Base.broadcastable(q::AbstractUnionQuantity) = new_quantity(typeof(q), Base.broadcastable(ustrip(q)), dimension(q))
for (type, _) in ABSTRACT_QUANTITY_TYPES
    @eval Base.getindex(q::$type) = new_quantity(typeof(q), getindex(ustrip(q)), dimension(q))
    @eval Base.getindex(q::$type, i::Integer...) = new_quantity(typeof(q), getindex(ustrip(q), i...), dimension(q))
    type == AbstractGenericQuantity &&
        @eval Base.getindex(q::$type, i...) = new_quantity(typeof(q), getindex(ustrip(q), i...), dimension(q))
end
Base.keys(q::AbstractUnionQuantity) = keys(ustrip(q))


# Numeric checks
function Base.isapprox(l::AbstractUnionQuantity, r::AbstractUnionQuantity; kws...)
    return isapprox(ustrip(l), ustrip(r); kws...) && dimension(l) == dimension(r)
end
function Base.isapprox(l::Number, r::AbstractUnionQuantity; kws...)
    iszero(dimension(r)) || throw(DimensionError(l, r))
    return isapprox(l, ustrip(r); kws...)
end
function Base.isapprox(l::AbstractUnionQuantity, r::Number; kws...)
    iszero(dimension(l)) || throw(DimensionError(l, r))
    return isapprox(ustrip(l), r; kws...)
end
Base.iszero(d::AbstractDimensions) = all_dimensions(iszero, d)
Base.:(==)(l::AbstractDimensions, r::AbstractDimensions) = all_dimensions(==, l, r)
Base.:(==)(l::AbstractUnionQuantity, r::AbstractUnionQuantity) = ustrip(l) == ustrip(r) && dimension(l) == dimension(r)
Base.:(==)(l::Number, r::AbstractUnionQuantity) = ustrip(l) == ustrip(r) && iszero(dimension(r))
Base.:(==)(l::AbstractUnionQuantity, r::Number) = ustrip(l) == ustrip(r) && iszero(dimension(l))
function Base.isless(l::AbstractUnionQuantity, r::AbstractUnionQuantity)
    dimension(l) == dimension(r) || throw(DimensionError(l, r))
    return isless(ustrip(l), ustrip(r))
end
function Base.isless(l::AbstractUnionQuantity, r::Number)
    iszero(dimension(l)) || throw(DimensionError(l, r))
    return isless(ustrip(l), r)
end
function Base.isless(l::Number, r::AbstractUnionQuantity)
    iszero(dimension(r)) || throw(DimensionError(l, r))
    return isless(l, ustrip(r))
end

# Simple flags:
for f in (:iszero, :isfinite, :isinf, :isnan, :isreal)
    @eval Base.$f(q::AbstractUnionQuantity) = $f(ustrip(q))
end

# Simple operations which return a full quantity (same dimensions)
for f in (:real, :imag, :conj, :adjoint, :unsigned, :nextfloat, :prevfloat)
    @eval Base.$f(q::AbstractUnionQuantity) = new_quantity(typeof(q), $f(ustrip(q)), dimension(q))
end

# Base.one, typemin, typemax
for f in (:one, :typemin, :typemax)
    @eval begin
        Base.$f(::Type{Q}) where {T,D,Q<:AbstractUnionQuantity{T,D}} = new_quantity(Q, $f(T), D)
        Base.$f(::Type{Q}) where {T,Q<:AbstractUnionQuantity{T}} = $f(constructor_of(Q){T, DEFAULT_DIM_TYPE})
        Base.$f(::Type{Q}) where {Q<:AbstractUnionQuantity} = $f(Q{DEFAULT_VALUE_TYPE, DEFAULT_DIM_TYPE})
    end
    if f == :one  # Return empty dimensions, as should be multiplicative identity.
        @eval Base.$f(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, $f(ustrip(q)), one(dimension(q)))
    else
        @eval Base.$f(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, $f(ustrip(q)), dimension(q))
    end
end
Base.one(::Type{D}) where {D<:AbstractDimensions} = D()
Base.one(::D) where {D<:AbstractDimensions} = one(D)

# Additive identities (zero)
Base.zero(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, zero(ustrip(q)), dimension(q))
Base.zero(::AbstractDimensions) = error("There is no such thing as an additive identity for a `AbstractDimensions` object, as + is only defined for `AbstractUnionQuantity`.")
Base.zero(::Type{<:AbstractUnionQuantity}) = error("Cannot create an additive identity for a `AbstractUnionQuantity` type, as the dimensions are unknown. Please use `zero(::AbstractUnionQuantity)` instead.")
Base.zero(::Type{<:AbstractDimensions}) = error("There is no such thing as an additive identity for a `AbstractDimensions` type, as + is only defined for `AbstractUnionQuantity`.")

# Dimensionful 1 (oneunit)
Base.oneunit(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, oneunit(ustrip(q)), dimension(q))
Base.oneunit(::AbstractDimensions) = error("There is no such thing as a dimensionful 1 for a `AbstractDimensions` object, as + is only defined for `AbstractUnionQuantity`.")
Base.oneunit(::Type{<:AbstractUnionQuantity}) = error("Cannot create a dimensionful 1 for a `AbstractUnionQuantity` type without knowing the dimensions. Please use `oneunit(::AbstractUnionQuantity)` instead.")
Base.oneunit(::Type{<:AbstractDimensions}) = error("There is no such thing as a dimensionful 1 for a `AbstractDimensions` type, as + is only defined for `AbstractUnionQuantity`.")

Base.show(io::IO, d::AbstractDimensions) =
    let tmp_io = IOBuffer()
        for k in filter(k -> !iszero(d[k]), keys(d))
            print(tmp_io, dimension_name(d, k))
            isone(d[k]) || pretty_print_exponent(tmp_io, d[k])
            print(tmp_io, " ")
        end
        s = String(take!(tmp_io))
        s = replace(s, r"^\s*" => "")
        s = replace(s, r"\s*$" => "")
        print(io, s)
    end
Base.show(io::IO, q::AbstractUnionQuantity{<:Real}) = print(io, ustrip(q), " ", dimension(q))
Base.show(io::IO, q::AbstractUnionQuantity) = print(io, "(", ustrip(q), ") ", dimension(q))

function dimension_name(::AbstractDimensions, k::Symbol)
    default_dimensions = (length="m", mass="kg", time="s", current="A", temperature="K", luminosity="cd", amount="mol")
    return get(default_dimensions, k, string(k))
end

string_rational(x) = isinteger(x) ? string(round(Int, x)) : string(x)
pretty_print_exponent(io::IO, x) = print(io, to_superscript(string_rational(x)))
const SUPERSCRIPT_MAPPING = ('⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹')
const INTCHARS = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
to_superscript(s::AbstractString) = join(
    map(replace(s, "//" => "ᐟ")) do c
        if c ∈ INTCHARS
            SUPERSCRIPT_MAPPING[parse(Int, c)+1]
        elseif c == '-'
            '⁻'
        else
            c
        end
    end
)

tryrationalize(::Type{R}, x::R) where {R} = x
tryrationalize(::Type{R}, x::Union{Rational,Integer}) where {R} = convert(R, x)
tryrationalize(::Type{R}, x) where {R} = isinteger(x) ? convert(R, round(Int, x)) : convert(R, rationalize(Int, x))

Base.showerror(io::IO, e::DimensionError) = print(io, "DimensionError: ", e.q1, " and ", e.q2, " have incompatible dimensions")

# TODO: these are redundant with the constructors
Base.convert(::Type{Q}, q::AbstractUnionQuantity) where {Q<:AbstractUnionQuantity} = q
Base.convert(::Type{Q}, q::AbstractUnionQuantity) where {T,Q<:AbstractUnionQuantity{T}} = new_quantity(Q, convert(T, ustrip(q)), dimension(q))
Base.convert(::Type{Q}, q::AbstractUnionQuantity) where {T,D,Q<:AbstractUnionQuantity{T,D}} = new_quantity(Q, convert(T, ustrip(q)), convert(D, dimension(q)))

Base.convert(::Type{D}, d::AbstractDimensions) where {D<:AbstractDimensions} = d
Base.convert(::Type{D}, d::AbstractDimensions) where {R,D<:AbstractDimensions{R}} = D(d)

Base.copy(d::D) where {D<:AbstractDimensions} = map_dimensions(copy, d)
Base.copy(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, copy(ustrip(q)), copy(dimension(q)))

"""
    ustrip(q::AbstractUnionQuantity)

Remove the units from a quantity.
"""
@inline ustrip(q::AbstractUnionQuantity) = q.value
ustrip(::AbstractDimensions) = error("Cannot remove units from an `AbstractDimensions` object.")
@inline ustrip(q) = q

"""
    dimension(q::AbstractUnionQuantity)

Get the dimensions of a quantity, returning an `AbstractDimensions` object.
"""
dimension(q::AbstractUnionQuantity) = q.dimensions
dimension(d::AbstractDimensions) = d
dimension(aq::AbstractArray{<:AbstractUnionQuantity}) = allequal(dimension.(aq)) ? dimension(first(aq)) : throw(DimensionError(aq[begin], aq[begin+1:end]))

"""
    ulength(q::AbstractUnionQuantity)
    ulength(d::AbstractDimensions)

Get the length dimension of a quantity (e.g., meters^(ulength)).
"""
ulength(q::AbstractUnionQuantity) = ulength(dimension(q))
ulength(d::AbstractDimensions) = d.length

"""
    umass(q::AbstractUnionQuantity)
    umass(d::AbstractDimensions)

Get the mass dimension of a quantity (e.g., kg^(umass)).
"""
umass(q::AbstractUnionQuantity) = umass(dimension(q))
umass(d::AbstractDimensions) = d.mass

"""
    utime(q::AbstractUnionQuantity)
    utime(d::AbstractDimensions)

Get the time dimension of a quantity (e.g., s^(utime))
"""
utime(q::AbstractUnionQuantity) = utime(dimension(q))
utime(d::AbstractDimensions) = d.time

"""
    ucurrent(q::AbstractUnionQuantity)
    ucurrent(d::AbstractDimensions)

Get the current dimension of a quantity (e.g., A^(ucurrent)).
"""
ucurrent(q::AbstractUnionQuantity) = ucurrent(dimension(q))
ucurrent(d::AbstractDimensions) = d.current

"""
    utemperature(q::AbstractUnionQuantity)
    utemperature(d::AbstractDimensions)

Get the temperature dimension of a quantity (e.g., K^(utemperature)).
"""
utemperature(q::AbstractUnionQuantity) = utemperature(dimension(q))
utemperature(d::AbstractDimensions) = d.temperature

"""
    uluminosity(q::AbstractUnionQuantity)
    uluminosity(d::AbstractDimensions)

Get the luminosity dimension of a quantity (e.g., cd^(uluminosity)).
"""
uluminosity(q::AbstractUnionQuantity) = uluminosity(dimension(q))
uluminosity(d::AbstractDimensions) = d.luminosity

"""
    uamount(q::AbstractUnionQuantity)
    uamount(d::AbstractDimensions)

Get the amount dimension of a quantity (e.g., mol^(uamount)).
"""
uamount(q::AbstractUnionQuantity) = uamount(dimension(q))
uamount(d::AbstractDimensions) = d.amount
