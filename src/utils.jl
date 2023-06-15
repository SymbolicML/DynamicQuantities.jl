import Tricks: static_fieldnames

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

Base.float(q::AbstractQuantity{T}) where {T<:AbstractFloat} = convert(T, q)
Base.convert(::Type{T}, q::AbstractQuantity) where {T<:Real} =
    let
        @assert iszero(q.dimensions) "Quantity $(q) has dimensions! Use `ustrip` instead."
        return convert(T, q.value)
    end

Base.isfinite(q::AbstractQuantity) = isfinite(ustrip(q))
Base.keys(d::AbstractDimensions) = static_fieldnames(typeof(d))
# TODO: Make this more generic.
Base.iszero(d::AbstractDimensions) = all_dimensions(iszero, d)
Base.iszero(q::AbstractQuantity) = iszero(ustrip(q))
Base.getindex(d::AbstractDimensions, k::Symbol) = getfield(d, k)
Base.:(==)(l::AbstractDimensions, r::AbstractDimensions) = all_dimensions(==, l, r)
Base.:(==)(l::AbstractQuantity, r::AbstractQuantity) = ustrip(l) == ustrip(r) && dimension(l) == dimension(r)
Base.:(==)(l, r::AbstractQuantity) = ustrip(l) == ustrip(r) && dimension(l) == dimension(r)
Base.:(==)(l::AbstractQuantity, r) = ustrip(l) == ustrip(r) && dimension(l) == dimension(r)
Base.isless(l::AbstractQuantity, r::AbstractQuantity) = dimension(l) == dimension(r) ? isless(ustrip(l), ustrip(r)) : throw(DimensionError(l, r))
Base.isless(l::AbstractQuantity, r) = dimension(l) == dimension(r) ? isless(ustrip(l), r) : throw(DimensionError(l, r))
Base.isless(l, r::AbstractQuantity) = dimension(l) == dimension(r) ? isless(l, ustrip(r)) : throw(DimensionError(l, r))
Base.isapprox(l::AbstractQuantity, r::AbstractQuantity; kws...) = isapprox(ustrip(l), ustrip(r); kws...) && dimension(l) == dimension(r)
Base.length(::AbstractDimensions) = 1
Base.length(::AbstractQuantity) = 1
Base.iterate(d::AbstractDimensions) = (d, nothing)
Base.iterate(::AbstractDimensions, ::Nothing) = nothing
Base.iterate(q::AbstractQuantity) = (q, nothing)
Base.iterate(::AbstractQuantity, ::Nothing) = nothing

# Multiplicative identities:
Base.one(::Type{Quantity{T,R}}) where {T,R} = Quantity(one(T), R)
Base.one(::Type{Quantity{T}}) where {T} = one(Quantity{T,DEFAULT_DIM_TYPE})
Base.one(::Type{Quantity}) = one(Quantity{DEFAULT_VALUE_TYPE})
Base.one(::Type{Dimensions{R}}) where {R} = Dimensions{R}()
Base.one(::Type{Dimensions}) = one(Dimensions{DEFAULT_DIM_TYPE})
Base.one(q::Quantity) = Quantity(one(ustrip(q)), one(dimension(q)))
Base.one(d::Dimensions) = one(typeof(d))

# Additive identities:
Base.zero(q::Quantity) = Quantity(zero(ustrip(q)), dimension(q))
Base.zero(::Dimensions) = error("There is no such thing as an additive identity for a `Dimensions` object, as + is only defined for `Quantity`.")
Base.zero(::Type{<:Quantity}) = error("Cannot create an additive identity for a `Quantity` type, as the dimensions are unknown. Please use `zero(::Quantity)` instead.")
Base.zero(::Type{<:Dimensions}) = error("There is no such thing as an additive identity for a `Dimensions` type, as + is only defined for `Quantity`.")

# Dimensionful 1:
Base.oneunit(q::Quantity) = Quantity(oneunit(ustrip(q)), dimension(q))
Base.oneunit(::Dimensions) = error("There is no such thing as a dimensionful 1 for a `Dimensions` object, as + is only defined for `Quantity`.")
Base.oneunit(::Type{<:Quantity}) = error("Cannot create a dimensionful 1 for a `Quantity` type without knowing the dimensions. Please use `oneunit(::Quantity)` instead.")
Base.oneunit(::Type{<:Dimensions}) = error("There is no such thing as a dimensionful 1 for a `Dimensions` type, as + is only defined for `Quantity`.")

Base.show(io::IO, d::AbstractDimensions) =
    let tmp_io = IOBuffer()
        for k in keys(d)
            if !iszero(d[k])
                print(tmp_io, dimension_name(d, k))
                isone(d[k]) || pretty_print_exponent(tmp_io, d[k])
                print(tmp_io, " ")
            end
        end
        s = String(take!(tmp_io))
        s = replace(s, r"^\s*" => "")
        s = replace(s, r"\s*$" => "")
        print(io, s)
    end
Base.show(io::IO, q::AbstractQuantity) = print(io, ustrip(q), " ", dimension(q))

string_rational(x) = isinteger(x) ? string(round(Int, x)) : string(x)
pretty_print_exponent(io::IO, x) = print(io, to_superscript(string_rational(x)))
const SUPERSCRIPT_MAPPING = ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹']
const INTCHARS = ['0' + i for i = 0:9]
to_superscript(s::AbstractString) = join(
    map(replace(replace(s, "-" => "⁻"), r"//" => "ᐟ")) do c
        c ∈ INTCHARS ? SUPERSCRIPT_MAPPING[parse(Int, c)+1] : c
    end
)

tryrationalize(::Type{R}, x::R) where {R} = x
tryrationalize(::Type{R}, x::Union{Rational,Integer}) where {R} = convert(R, x)
tryrationalize(::Type{R}, x) where {R} = isinteger(x) ? convert(R, round(Int, x)) : convert(R, rationalize(Int, x))

Base.showerror(io::IO, e::DimensionError) = print(io, "DimensionError: ", e.q1, " and ", e.q2, " have incompatible dimensions")

Base.convert(::Type{Quantity}, q::Quantity) = q
Base.convert(::Type{Quantity{T}}, q::Quantity) where {T} = Quantity{T}(q)
Base.convert(::Type{Quantity{T,R}}, q::Quantity) where {T,R} = Quantity{T,R}(q)

Base.convert(::Type{Dimensions}, d::Dimensions) = d
Base.convert(::Type{Dimensions{R}}, d::Dimensions) where {R} = Dimensions{R}(d)

"""
    ustrip(q::AbstractQuantity)

Remove the units from a quantity.
"""
ustrip(q::AbstractQuantity) = q.value
ustrip(::AbstractDimensions) = error("Cannot remove units from a `Dimensions` object.")
ustrip(q) = q

"""
    dimension(q::AbstractQuantity)

Get the dimensions of a quantity, returning a `Dimensions` object.
"""
dimension(q::AbstractQuantity) = q.dimensions
dimension(d::AbstractDimensions) = d
dimension(_) = Dimensions()
# TODO: Should we throw an error instead, because this is assuming a type?

"""
    ulength(q::AbstractQuantity)
    ulength(d::AbstractDimensions)

Get the length dimension of a quantity (e.g., meters^(ulength)).
"""
ulength(q::AbstractQuantity) = ulength(dimension(q))
ulength(d::AbstractDimensions) = d.length

"""
    umass(q::AbstractQuantity)
    umass(d::AbstractDimensions)

Get the mass dimension of a quantity (e.g., kg^(umass)).
"""
umass(q::AbstractQuantity) = umass(dimension(q))
umass(d::AbstractDimensions) = d.mass

"""
    utime(q::AbstractQuantity)
    utime(d::AbstractDimensions)

Get the time dimension of a quantity (e.g., s^(utime))
"""
utime(q::AbstractQuantity) = utime(dimension(q))
utime(d::AbstractDimensions) = d.time

"""
    ucurrent(q::AbstractQuantity)
    ucurrent(d::AbstractDimensions)

Get the current dimension of a quantity (e.g., A^(ucurrent)).
"""
ucurrent(q::AbstractQuantity) = ucurrent(dimension(q))
ucurrent(d::AbstractDimensions) = d.current

"""
    utemperature(q::AbstractQuantity)
    utemperature(d::AbstractDimensions)

Get the temperature dimension of a quantity (e.g., K^(utemperature)).
"""
utemperature(q::AbstractQuantity) = utemperature(dimension(q))
utemperature(d::AbstractDimensions) = d.temperature

"""
    uluminosity(q::AbstractQuantity)
    uluminosity(d::AbstractDimensions)

Get the luminosity dimension of a quantity (e.g., cd^(uluminosity)).
"""
uluminosity(q::AbstractQuantity) = uluminosity(dimension(q))
uluminosity(d::AbstractDimensions) = d.luminosity

"""
    uamount(q::AbstractQuantity)
    uamount(d::AbstractDimensions)

Get the amount dimension of a quantity (e.g., mol^(uamount)).
"""
uamount(q::AbstractQuantity) = uamount(dimension(q))
uamount(d::AbstractDimensions) = d.amount
