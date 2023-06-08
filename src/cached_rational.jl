const gcd_table =
    Tuple(
        Tuple(
            if j == 0
                (INT_TYPE(0), INT_TYPE(0))
            else
                let (out1, out2) = Base.divgcd(i, j)
                    (INT_TYPE(out1), INT_TYPE(out2))
                end
            end
            for i = (Int(typemin(INT_TYPE))+1):Int(typemax(INT_TYPE))
        )
        for j = (Int(typemin(INT_TYPE))+1):Int(typemax(INT_TYPE))
    );

# Pirate divgcd to speed it up for INT_TYPE
function cached_divgcd(x::INT_TYPE, y::INT_TYPE)
    @inbounds gcd_table[Int(y)+128][Int(x)+128]
end

"""Cached rational number"""
struct CRational{T} <: Real
    num::T
    den::T
    global unsafe_rational(num::Integer, den::Integer) = new{INT_TYPE}(INT_TYPE(num), INT_TYPE(den))
end

CRational(num::INT_TYPE, den::INT_TYPE) =
    let (num, den) = cached_divgcd(num, den)
        return unsafe_rational(num, den)
    end
CRational(n::INT_TYPE) = unsafe_rational(n, one(n))
CRational(n::Integer) = CRational(INT_TYPE(n))
CRational(r::Rational{INT_TYPE}) = unsafe_rational(r.num, r.den)
CRational(r::Rational) = unsafe_rational(INT_TYPE(r.num), INT_TYPE(r.den))

CRational{INT_TYPE}(n::Integer) = CRational(n)
CRational{INT_TYPE}(r::Rational) = CRational(r)

Base.:*(x::CRational, y::CRational) = CRational(x.num * y.num, x.den * y.den)
Base.:+(x::CRational, y::CRational) =
    let (xd, yd) = cached_divgcd(x.den, y.den)
        CRational(x.num * yd + y.num * xd, x.den * yd)
    end
Base.:-(x::CRational) = unsafe_rational(-x.num, x.den)
Base.:-(x::CRational, y::CRational) = x + (-y)
Base.:(==)(x::CRational, y::CRational) = (x.den == y.den) & (x.num == y.num)
Base.iszero(x::CRational) = iszero(x.num)
Base.isinteger(x::CRational) = x.den == one(INT_TYPE)
Base.convert(::Type{RI}, x::CRational) where {RI<:Rational} = RI(x.num, x.den)
Base.string(x::CRational) = string(convert(Rational, x))
Base.promote(x::CRational, y) = promote(convert(Rational, x), y)
Base.promote(x, y::CRational) = promote(x, convert(Rational, y))
