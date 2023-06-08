const gcd_table =
    Tuple(
        Tuple(
            if j == 0
                (Int8(0), Int8(0))
            else
                let (out1, out2) = Base.divgcd(Int8(i), Int8(j))
                    (Int8(out1), Int8(out2))
                end
            end
            for i = (typemin(Int8)+1):typemax(Int8)
        )
        for j = (typemin(Int8)+1):typemax(Int8)
    );

# Pirate divgcd to speed it up for Int8
function cached_divgcd(x::Int8, y::Int8)
    @inbounds gcd_table[y+128][x+128]
end

"""Cached rational number"""
struct CRational{T} <: Real
    num::T
    den::T
    global unsafe_rational(num::Integer, den::Integer) = new{Int8}(Int8(num), Int8(den))
end

CRational(num::Int8, den::Int8) =
    let (num, den) = cached_divgcd(num, den)
        return unsafe_rational(num, den)
    end
CRational(n::Int8) = unsafe_rational(n, one(n))
CRational(n::Integer) = CRational(Int8(n))
CRational(r::Rational{Int8}) = unsafe_rational(r.num, r.den)
CRational(r::Rational) = CRational(Int8(r.num), Int8(r.den))

CRational{Int8}(n::Integer) = CRational(n)
CRational{Int8}(r::Rational) = CRational(r)

Base.:*(x::CRational, y::CRational) = CRational(x.num * y.num, x.den * y.den)
Base.:+(x::CRational, y::CRational) =
    let (xd, yd) = cached_divgcd(x.den, y.den)
        CRational(x.num * yd + y.num * xd, x.den * yd)
    end
Base.:-(x::CRational) = unsafe_rational(-x.num, x.den)
Base.:-(x::CRational, y::CRational) = x + (-y)
Base.:(==)(x::CRational, y::CRational) = (x.den == y.den) & (x.num == y.num)
Base.iszero(x::CRational) = iszero(x.num)
Base.isinteger(x::CRational) = x.den == one(Int8)
Base.convert(::Type{RI}, x::CRational) where {RI<:Rational} = RI(x.num, x.den)
Base.string(x::CRational) = string(convert(Rational, x))
Base.promote(x::CRational, y) = promote(convert(Rational, x), y)
Base.promote(x, y::CRational) = promote(x, convert(Rational, y))
