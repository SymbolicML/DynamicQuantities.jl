# This is used to store floats without forcing promotion on other
# numeric types.
struct LazyFloat64 <: AbstractFloat
    value::Float64

    LazyFloat64(x::AbstractFloat) = new(convert(Float64, x))
end

Base.float(x::LazyFloat64) = x.value

Base.convert(::Type{LazyFloat64}, x::LazyFloat64) = x
Base.convert(::Type{LazyFloat64}, x::FixedRational) = LazyFloat64(convert(Float64, x))
Base.convert(::Type{LazyFloat64}, x::Number) = LazyFloat64(x)
Base.convert(::Type{T}, x::LazyFloat64) where {T<:Number} = convert(T, float(x))
Base.promote_rule(::Type{LazyFloat64}, ::Type{T}) where {T<:AbstractFloat} = T
Base.promote_rule(::Type{LazyFloat64}, ::Type{T}) where {T} = promote_type(Float64, T)

Base.show(io::IO, x::LazyFloat64) = print(io, float(x))

Base.:+(a::LazyFloat64, b::LazyFloat64) = LazyFloat64(float(a) + float(b))
Base.:-(a::LazyFloat64) = LazyFloat64(-float(a))
Base.:-(a::LazyFloat64, b::LazyFloat64) = LazyFloat64(float(a) - float(b))
Base.:*(a::LazyFloat64, b::LazyFloat64) = LazyFloat64(float(a) * float(b))
Base.inv(a::LazyFloat64) = LazyFloat64(inv(float(a)))
Base.abs(a::LazyFloat64) = LazyFloat64(abs(float(a)))
Base.:/(a::LazyFloat64, b::LazyFloat64) = a * inv(b)
Base.:^(a::LazyFloat64, b::Int) = LazyFloat64(float(a) ^ b)
Base.:^(a::LazyFloat64, b::LazyFloat64) = LazyFloat64(float(a) ^ float(b))
Base.sqrt(a::LazyFloat64) = LazyFloat64(sqrt(float(a)))
Base.cbrt(a::LazyFloat64) = LazyFloat64(cbrt(float(a)))
Base.eps(::Type{LazyFloat64}) = eps(Float64)

# Ambiguities:
for T in (:(Rational{<:Any}), :(Base.TwicePrecision), :AbstractChar, :Complex, :Number)
    @eval LazyFloat64(x::$T) = LazyFloat64(float(x))
end
