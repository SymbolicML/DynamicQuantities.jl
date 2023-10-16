# This is used to store floats without forcing promotion on other
# numeric types.
struct AutoFloat <: AbstractFloat
    value::Float64

    AutoFloat(x::AbstractFloat) = new(convert(Float64, x))
end

Base.float(x::AutoFloat) = x.value

Base.convert(::Type{AutoFloat}, x::AutoFloat) = x
Base.convert(::Type{AutoFloat}, x::FixedRational) = AutoFloat(convert(Float64, x))
Base.convert(::Type{AutoFloat}, x::Number) = AutoFloat(x)
Base.convert(::Type{T}, x::AutoFloat) where {T<:Number} = convert(T, float(x))
Base.promote_rule(::Type{AutoFloat}, ::Type{T}) where {T<:AbstractFloat} = T
Base.promote_rule(::Type{AutoFloat}, ::Type{T}) where {T} = promote_type(Float64, T)

Base.show(io::IO, x::AutoFloat) = print(io, float(x))

Base.:+(a::AutoFloat, b::AutoFloat) = AutoFloat(float(a) + float(b))
Base.:-(a::AutoFloat) = AutoFloat(-float(a))
Base.:-(a::AutoFloat, b::AutoFloat) = AutoFloat(float(a) - float(b))
Base.:*(a::AutoFloat, b::AutoFloat) = AutoFloat(float(a) * float(b))
Base.inv(a::AutoFloat) = AutoFloat(inv(float(a)))
Base.abs(a::AutoFloat) = AutoFloat(abs(float(a)))
Base.:/(a::AutoFloat, b::AutoFloat) = a * inv(b)
Base.:^(a::AutoFloat, b::Int) = AutoFloat(float(a) ^ b)
Base.:^(a::AutoFloat, b::AutoFloat) = AutoFloat(float(a) ^ float(b))
Base.sqrt(a::AutoFloat) = AutoFloat(sqrt(float(a)))
Base.cbrt(a::AutoFloat) = AutoFloat(cbrt(float(a)))
Base.eps(::Type{AutoFloat}) = eps(Float64)

# Ambiguities:
for T in (:(Rational{<:Any}), :(Base.TwicePrecision), :AbstractChar, :Complex, :Number)
    @eval AutoFloat(x::$T) = AutoFloat(float(x))
end
