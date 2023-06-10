"""
    FixedRational{T,den}

A rational number with a fixed denominator. Significantly
faster than `Rational{T}`, as it never needs to compute
the `gcd` apart from when printing.
"""
struct FixedRational{T<:Integer,den} <: Real
    num::T
    global unsafe_fixed_rational(num::Integer, ::Type{T}, ::Val{den}) where {T,den} = new{T,den}(num)
end
FixedRational{T,den}(x::Integer) where {T,den} = unsafe_fixed_rational(x * den, T, Val(den))
FixedRational{T,den}(x::Rational) where {T,den} = unsafe_fixed_rational(widemul(x.num, den) ÷ x.den, T, Val(den))

Base.:*(l::F, r::F) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(widemul(l.num, r.num) ÷ den, T, Val(den))
Base.:+(l::F, r::F) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(l.num + r.num, T, Val(den))
Base.:-(l::F, r::F) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(l.num - r.num, T, Val(den))
Base.:-(x::F) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(-x.num, T, Val(den))
Base.inv(x::F) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(widemul(den, den) ÷ x.num, T, Val(den))

Base.isinteger(x::F) where {T,den,F<:FixedRational{T,den}} = iszero(x.num % den)
Base.convert(::Type{FixedRational{T,den}}, x::Integer) where {T,den} = unsafe_fixed_rational(x * den, T, Val(den))
Base.convert(::Type{FixedRational{T,den}}, x::Rational) where {T,den} = FixedRational{T,den}(x)
Base.convert(::Type{Rational}, x::FixedRational{T,den}) where {T,den} = Rational{T}(x.num, den)
Base.convert(::Type{AF}, x::FixedRational{T,den}) where {AF<:AbstractFloat,T,den} = convert(AF, x.num) / convert(AF, den)
Base.round(::Type{T}, x::F) where {T,T2,den,F<:FixedRational{T2,den}} = div(convert(T, x.num), convert(T, den), RoundNearest)
Base.promote(x::T, y::F) where {T,T2,den,F<:FixedRational{T2,den}} = promote(x, convert(Rational, y))
Base.promote(x::F, y::T) where {T,T2,den,F<:FixedRational{T2,den}} = promote(convert(Rational, x), y)
Base.show(io::IO, x::F) where {T,den,F<:FixedRational{T,den}} = show(io, convert(Rational, x))
Base.zero(::Type{F}) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(0, T, Val(den))

tryrationalize(::Type{F}, x::F) where {T,den,F<:FixedRational{T,den}} = x
tryrationalize(::Type{F}, x::Union{Rational,Integer}) where {T,den,F<:FixedRational{T,den}} = convert(F, x)
tryrationalize(::Type{F}, x) where {T,den,F<:FixedRational{T,den}} = unsafe_fixed_rational(round(T, x * den), T, Val(den))
