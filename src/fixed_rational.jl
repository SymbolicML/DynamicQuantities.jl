"""
    FixedRational{T,den}

A rational number with a fixed denominator. Significantly
faster than `Rational{T}`, as it never needs to compute
the `gcd` apart from when printing.
Access the denominator with `denom(F)` (which converts to `T`).

# Fields

- `num`: numerator of type `T`. The denominator is fixed to the type parameter `den`.
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
denom(x::FixedRational) = denom(typeof(x))

# But, for Val(den), we need to use the same type as at init.
# Otherwise, we would have type instability.
val_denom(::Type{F}) where {T,den,F<:FixedRational{T,den}} = Val(den)

num_type(::Type{F}) where {T,F<:FixedRational{T}} = T

const DEFAULT_NUMERATOR_TYPE = Int32
const DEFAULT_DENOM = DEFAULT_NUMERATOR_TYPE(2^4 * 3^2 * 5^2 * 7)

(::Type{F})(x::F) where {F<:FixedRational} = x
(::Type{F})(x::F2) where {T,T2,den,F<:FixedRational{T,den},F2<:FixedRational{T2,den}} = unsafe_fixed_rational(x.num, num_type(F), val_denom(F))
(::Type{F})(x::Integer) where {F<:FixedRational} = unsafe_fixed_rational(x * denom(F), num_type(F), val_denom(F))
(::Type{F})(x::Rational) where {F<:FixedRational} = unsafe_fixed_rational(widemul(x.num, denom(F)) ÷ x.den, num_type(F), val_denom(F))

Base.:*(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(widemul(l.num, r.num) ÷ denom(F), num_type(F), val_denom(F))
Base.:+(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(l.num + r.num, num_type(F), val_denom(F))
Base.:-(l::F, r::F) where {F<:FixedRational} = unsafe_fixed_rational(l.num - r.num, num_type(F), val_denom(F))
Base.:-(x::F) where {F<:FixedRational} = unsafe_fixed_rational(-x.num, num_type(F), val_denom(F))

Base.inv(x::F) where {F<:FixedRational} = unsafe_fixed_rational(widemul(denom(F), denom(F)) ÷ x.num, num_type(F), val_denom(F))

Base.:*(l::F, r::Integer) where {F<:FixedRational} = unsafe_fixed_rational(l.num * r, num_type(F), val_denom(F))
Base.:*(l::Integer, r::F) where {F<:FixedRational} = unsafe_fixed_rational(l * r.num, num_type(F), val_denom(F))

for comp in (:(==), :isequal, :<, :(isless), :<=)
    @eval Base.$comp(x::F, y::F) where {F<:FixedRational} = $comp(x.num, y.num)
end

Base.iszero(x::FixedRational) = iszero(x.num)
Base.isone(x::F) where {F<:FixedRational} = x.num == denom(F)
Base.isinteger(x::F) where {F<:FixedRational} = iszero(x.num % denom(F))

Rational{R}(x::F) where {R,F<:FixedRational} = Rational{R}(x.num, denom(F))
Rational(x::F) where {F<:FixedRational} = Rational{num_type(F)}(x)
(::Type{AF})(x::F) where {AF<:AbstractFloat,F<:FixedRational} = convert(AF, x.num) / convert(AF, denom(F))
(::Type{I})(x::F) where {I<:Integer,F<:FixedRational} =
    let
        isinteger(x) || throw(InexactError(:convert, I, x))
        convert(I, div(x.num, denom(F)))
    end
(::Type{Bool})(x::F) where {F<:FixedRational} =
    let
        iszero(x) || isone(x) || throw(InexactError(:convert, Bool, x))
        return x.num == denom(F)
    end

Base.round(::Type{T}, x::F, r::RoundingMode=RoundNearest) where {T,F<:FixedRational} = div(convert(T, x.num), convert(T, denom(F)), r)
Base.decompose(x::F) where {T,F<:FixedRational{T}} = (x.num, zero(T), denom(F))

# Promotion with self or rational-like
function Base.promote_rule(::Type{F1}, ::Type{F2}) where {F1<:FixedRational,F2<:FixedRational}
    denom(F1) == denom(F2) ||
        error("Refusing to promote `FixedRational` types with mixed denominators. Use `Rational` instead.")
    return FixedRational{promote_type(num_type(F1), num_type(F2)),denom(F1)}
end
function Base.promote_rule(::Type{F}, ::Type{Rational{T2}}) where {F<:FixedRational,T2}
    return Rational{promote_type(num_type(F),T2)}
end
function Base.promote_rule(::Type{Rational{T2}}, ::Type{F}) where {F<:FixedRational,T2}
    return Rational{promote_type(num_type(F),T2)}
end

# We want to consume integers
function Base.promote_rule(::Type{F}, ::Type{<:Integer}) where {F<:FixedRational}
    return F
end
function Base.promote_rule(::Type{<:Integer}, ::Type{F}) where {F<:FixedRational}
    return F
end

# Promotion with general types promotes like a rational
function Base.promote_rule(::Type{T}, ::Type{T2}) where {T2<:Real,T<:FixedRational}
    return promote_type(Rational{num_type(T)}, T2)
end
function Base.promote_rule(::Type{T2}, ::Type{T}) where {T2<:Real,T<:FixedRational}
    return promote_type(Rational{num_type(T)}, T2)
end

Base.string(x::FixedRational) =
    let
        isinteger(x) && return string(convert(num_type(x), x))
        g = gcd(x.num, denom(x))
        return string(div(x.num, g)) * "//" * string(div(denom(x), g))
    end
Base.show(io::IO, x::FixedRational) = print(io, string(x))

tryrationalize(::Type{F}, x::F) where {F<:FixedRational} = x
tryrationalize(::Type{F}, x::Union{Rational,Integer}) where {F<:FixedRational} = convert(F, x)
tryrationalize(::Type{F}, x) where {F<:FixedRational} = unsafe_fixed_rational(round(num_type(F), x * denom(F)), num_type(F), val_denom(F))

# Fix method ambiguities
Base.round(::Type{T}, x::F, r::RoundingMode=RoundNearest) where {T>:Missing, F<:FixedRational} = round(Base.nonmissingtype_checked(T), x, r)
