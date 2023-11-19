for op in (:isless, :(==), :isequal, :(<)), (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        Base.$(op)(::$type, ::Missing) = missing
        Base.$(op)(::Missing, ::$type) = missing
    end
end
for op in (:isapprox,), (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        Base.$(op)(::$type, ::Missing; kws...) = missing
        Base.$(op)(::Missing, ::$type; kws...) = missing
    end
end

for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        Base.:(==)(::$type, ::WeakRef) = error("Cannot compare a quantity to a weakref")
        Base.:(==)(::WeakRef, ::$type) = error("Cannot compare a weakref to a quantity")
    end
end

Base.:*(l::AbstractDimensions, r::Number) = error("Please use an `UnionAbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:*(l::Number, r::AbstractDimensions) = error("Please use an `UnionAbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::AbstractDimensions, r::Number) = error("Please use an `UnionAbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::Number, r::AbstractDimensions) = error("Please use an `UnionAbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")

# Promotion ambiguities
function Base.promote_rule(::Type{F}, ::Type{Bool}) where {F<:FixedRational}
    return F
end
function Base.promote_rule(::Type{Bool}, ::Type{F}) where {F<:FixedRational}
    return F
end
function Base.promote_rule(::Type{F}, ::Type{BigFloat}) where {F<:FixedRational}
    return promote_type(Rational{eltype(F)}, BigFloat)
end
function Base.promote_rule(::Type{BigFloat}, ::Type{F}) where {F<:FixedRational}
    return promote_type(Rational{eltype(F)}, BigFloat)
end
function Base.promote_rule(::Type{F}, ::Type{T}) where {F<:FixedRational,T<:AbstractIrrational}
    return promote_type(Rational{eltype(F)}, T)
end
function Base.promote_rule(::Type{T}, ::Type{F}) where {F<:FixedRational,T<:AbstractIrrational}
    return promote_type(Rational{eltype(F)}, T)
end

# Assorted calls found by Aqua:
for type in (Signed, Float64, Float32, Rational), op in (:flipsign, :copysign)
    @eval function Base.$(op)(x::$type, y::AbstractRealQuantity)
        return $(op)(x, ustrip(y))
    end
end

function Base.:*(l::Complex{Bool}, r::AbstractRealQuantity)
    return new_quantity(typeof(r), l * ustrip(r), dimension(r))
end
function Base.:*(l::AbstractRealQuantity, r::Complex{Bool})
    return new_quantity(typeof(l), ustrip(l) * r, dimension(l))
end

for op in (:(==), :isequal), base_type in (AbstractIrrational, AbstractFloat)
    @eval begin
        function Base.$(op)(l::AbstractRealQuantity, r::$base_type)
            return $(op)(ustrip(l), r) && iszero(dimension(l))
        end
        function Base.$(op)(l::$base_type, r::AbstractRealQuantity)
            return $(op)(l, ustrip(r)) && iszero(dimension(r))
        end
    end
end

function Base.isless(l::AbstractRealQuantity, r::AbstractFloat)
    iszero(dimension(l)) || throw(DimensionError(l, r))
    return isless(ustrip(l), r)
end
function Base.isless(l::AbstractFloat, r::AbstractRealQuantity)
    iszero(dimension(r)) || throw(DimensionError(l, r))
    return isless(l, ustrip(r))
end
