macro map_dimensions(f, l...)
    # Create a new Dimensions object by applying f to each key
    output = :(Dimensions())
    for dim in DIMENSION_NAMES
        f_expr = :($f())
        for arg in l
            push!(f_expr.args, :($arg.$dim))
        end
        push!(output.args, f_expr)
    end
    return output |> esc
end
macro all_dimensions(f, l...)
    # Test a function over all dimensions
    output = Expr(:&&)
    for dim in DIMENSION_NAMES
        f_expr = :($f())
        for arg in l
            push!(f_expr.args, :($arg.$dim))
        end
        push!(output.args, f_expr)
    end
    return output |> esc
end

Base.float(q::Quantity{T}) where {T<:AbstractFloat} = convert(T, q)
Base.convert(::Type{T}, q::Quantity) where {T<:Real} =
    let
        @assert q.valid "Quantity $(q) is invalid!"
        @assert iszero(q.dimensions) "Quantity $(q) has dimensions! Use `ustrip` instead."
        return convert(T, q.value)
    end

Base.isfinite(q::Quantity) = isfinite(q.value)
Base.keys(::Dimensions) = DIMENSION_NAMES
Base.iszero(d::Dimensions) = @all_dimensions(iszero, d)
Base.iszero(q::Quantity) = iszero(q.value)
Base.getindex(d::Dimensions, k::Symbol) = getfield(d, k)
Base.:(==)(l::Dimensions, r::Dimensions) = @all_dimensions(==, l, r)
Base.:(==)(l::Quantity, r::Quantity) = l.value == r.value && l.dimensions == r.dimensions && l.valid == r.valid
Base.:(≈)(l::Quantity, r::Quantity) = l.value ≈ r.value && l.dimensions == r.dimensions && l.valid == r.valid
Base.length(::Dimensions) = 1
Base.length(::Quantity) = 1
Base.iterate(d::Dimensions) = (d, nothing)
Base.iterate(::Dimensions, ::Nothing) = nothing
Base.iterate(q::Quantity) = (q, nothing)
Base.iterate(::Quantity, ::Nothing) = nothing
Base.zero(::Type{Quantity{T}}) where {T} = Quantity(zero(T))
Base.one(::Type{Quantity{T}}) where {T} = Quantity(one(T))
Base.one(::Type{Dimensions}) = Dimensions()

Base.show(io::IO, d::Dimensions) =
    let tmp_io = IOBuffer()
        for k in keys(d)
            if !iszero(d[k])
                print(tmp_io, SYNONYM_MAPPING[k])
                pretty_print_exponent(tmp_io, d[k])
                print(tmp_io, " ")
            end
        end
        s = String(take!(tmp_io))
        s = replace(s, r"^\s*" => "")
        s = replace(s, r"\s*$" => "")
        print(io, s)
    end
Base.show(io::IO, q::Quantity) = q.valid ? print(io, q.value, " ", q.dimensions) : print(io, "INVALID")

string_rational(x::CRational) = isinteger(x) ? string(x.num) : string(x)
pretty_print_exponent(io::IO, x::R) =
    let
        print(io, " ", to_superscript(string_rational(x)))
    end
const SUPERSCRIPT_MAPPING = ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹']
const INTCHARS = ['0' + i for i = 0:9]
to_superscript(s::AbstractString) = join(
    map(replace(replace(s, "-" => "⁻"), r"//" => "ᐟ")) do c
        c ∈ INTCHARS ? SUPERSCRIPT_MAPPING[parse(Int, c)+1] : c
    end
)

tryrationalize(::Type{RI}, x::RI) where {RI} = x
tryrationalize(::Type{RI}, x::Rational) where {RI} = RI(x)
tryrationalize(::Type{RI}, x::Integer) where {RI} = RI(x)
tryrationalize(::Type{RI}, x) where {RI} = simple_ratio_rationalize(RI, x)
simple_ratio_rationalize(::Type{RI}, x) where {RI} =
    let int_type = RI.parameters[1]
        isinteger(x) ? RI(round(int_type, x)) : RI(rationalize(int_type, x))
    end

"""
    ustrip(q::Quantity)

Remove the units from a quantity.
"""
ustrip(q::Quantity) = q.value
ustrip(q::Number) = q

"""
    dimension(q::Quantity)

Get the dimensions of a quantity, returning a `Dimensions` object.
"""
dimension(q::Quantity) = q.dimensions
dimension(::Number) = Dimensions()

"""
    valid(q::Quantity)

Check if a quantity is valid. If invalid, dimensional analysis
during a previous calculation failed (such as adding mass and velocity).
"""
valid(q::Quantity) = q.valid

"""
    ulength(q::Quantity)

Get the length dimension of a quantity (e.g., meters^(ulength)).
"""
ulength(q::Quantity) = q.dimensions.length

"""
    umass(q::Quantity)

Get the mass dimension of a quantity (e.g., kg^(umass)).
"""
umass(q::Quantity) = q.dimensions.mass

"""
    utime(q::Quantity)

Get the time dimension of a quantity (e.g., s^(utime))
"""
utime(q::Quantity) = q.dimensions.time

"""
    ucurrent(q::Quantity)

Get the current dimension of a quantity (e.g., A^(ucurrent)).
"""
ucurrent(q::Quantity) = q.dimensions.current

"""
    utemperature(q::Quantity)

Get the temperature dimension of a quantity (e.g., K^(utemperature)).
"""
utemperature(q::Quantity) = q.dimensions.temperature

"""
    uluminosity(q::Quantity)

Get the luminosity dimension of a quantity (e.g., cd^(uluminosity)).
"""
uluminosity(q::Quantity) = q.dimensions.luminosity

"""
    uamount(q::Quantity)

Get the amount dimension of a quantity (e.g., mol^(uamount)).
"""
uamount(q::Quantity) = q.dimensions.amount
