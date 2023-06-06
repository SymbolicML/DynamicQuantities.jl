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

tryround(x::Rational{Int}) = isinteger(x) ? round(Int, x) : x
pretty_print_exponent(io::IO, x::Rational{Int}) =
    let
        if isinteger(x)
            print(io, " ", to_superscript(string(round(Int, x))))
        else
            print(io, " ", to_superscript(string(x)))
        end
    end
const SUPERSCRIPT_MAPPING = ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹']
to_superscript(s::AbstractString) = replace(s, r"\d" => x -> SUPERSCRIPT_MAPPING[parse(Int, x)+1], r"//" => "ᐟ", "-" => "⁻")

tryrationalize(::Type{T}, x::Rational{T}) where {T<:Integer} = x
tryrationalize(::Type{T}, x::T) where {T<:Integer} = Rational{T}(x)
tryrationalize(::Type{T}, x) where {T<:Integer} = rationalize(T, x)

ustrip(q::Quantity) = q.value
ustrip(q::Number) = q
dimension(q::Quantity) = q.dimensions
dimension(::Number) = Dimensions()
valid(q::Quantity) = q.valid

ulength(q::Quantity) = q.dimensions.length
umass(q::Quantity) = q.dimensions.mass
utime(q::Quantity) = q.dimensions.time
ucurrent(q::Quantity) = q.dimensions.current
utemperature(q::Quantity) = q.dimensions.temperature
uluminosity(q::Quantity) = q.dimensions.luminosity
uamount(q::Quantity) = q.dimensions.amount
