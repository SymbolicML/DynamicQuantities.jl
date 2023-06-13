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

"""
    denom(F::FixedRational)

Since `den` can be a different type than `T`, this function
is used to get the denominator as a `T`.
"""
denom(::Type{F}) where {T,den,F<:FixedRational{T,den}} = convert(T, den)

# But, for Val(den), we need to use the same type as at init.
# Otherwise, we would have type instability.
val_denom(::Type{F}) where {T,den,F<:FixedRational{T,den}} = Val(den)

Base.eltype(::Type{F}) where {T,den,F<:FixedRational{T,den}} = T

(::Type{F})(x::Integer) where {F<:FixedRational} = unsafe_fixed_rational(x * denom(F), eltype(F), val_denom(F))
(::Type{F})(x::Rational) where {F<:FixedRational} = unsafe_fixed_rational(widemul(x.num, denom(F)) ÷ x.den, eltype(F), val_denom(F))

Base.:*(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(widemul(l.num, r.num) ÷ denom(F), eltype(F), val_denom(F))
Base.:+(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(l.num + r.num, eltype(F), val_denom(F))
Base.:-(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(l.num - r.num, eltype(F), val_denom(F))
Base.:-(x::F) where {F<:FixedRational} = unsafe_fixed_rational(-x.num, eltype(F), val_denom(F))
Base.inv(x::F) where {F<:FixedRational} = unsafe_fixed_rational(widemul(denom(F), denom(F)) ÷ x.num, eltype(F), val_denom(F))

Base.:(==)(x::F, y::F) where {F<:FixedRational} = x.num == y.num
Base.iszero(x::FixedRational) = iszero(x.num)
Base.isone(x::F) where {F<:FixedRational} = x.num == denom(F)
Base.isinteger(x::F) where {F<:FixedRational} = iszero(x.num % denom(F))
Base.convert(::Type{F}, x::Integer) where {F<:FixedRational} = unsafe_fixed_rational(x * denom(F), eltype(F), val_denom(F))
Base.convert(::Type{F}, x::Rational) where {F<:FixedRational} = F(x)
Base.convert(::Type{Rational{R}}, x::F) where {R,F<:FixedRational} = Rational{R}(x.num, denom(F))
Base.convert(::Type{Rational}, x::F) where {F<:FixedRational} = Rational{eltype(F)}(x.num, denom(F))
Base.convert(::Type{AF}, x::F) where {AF<:AbstractFloat,F<:FixedRational} = convert(AF, x.num) / convert(AF, denom(F))
Base.round(::Type{T}, x::F) where {T,F<:FixedRational} = div(convert(T, x.num), convert(T, denom(F)), RoundNearest)
Base.promote(x::Integer, y::F) where {F<:FixedRational} = (F(x), y)
Base.promote(x::F, y::Integer) where {F<:FixedRational} = (x, F(y))
Base.promote(x, y::F) where {F<:FixedRational} = promote(x, convert(Rational, y))
Base.promote(x::F, y) where {F<:FixedRational} = promote(convert(Rational, x), y)
Base.show(io::IO, x::F) where {F<:FixedRational} = show(io, convert(Rational, x))
Base.zero(::Type{F}) where {F<:FixedRational} = unsafe_fixed_rational(0, eltype(F), val_denom(F))

tryrationalize(::Type{F}, x::F) where {F<:FixedRational} = x
tryrationalize(::Type{F}, x::Union{Rational,Integer}) where {F<:FixedRational} = convert(F, x)
tryrationalize(::Type{F}, x) where {F<:FixedRational} = unsafe_fixed_rational(round(eltype(F), x * denom(F)), eltype(F), val_denom(F))
