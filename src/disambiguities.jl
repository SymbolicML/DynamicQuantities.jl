Base.isless(::AbstractQuantity, ::Missing) = missing
Base.isless(::Missing, ::AbstractQuantity) = missing
Base.:(==)(::AbstractQuantity, ::Missing) = missing
Base.:(==)(::Missing, ::AbstractQuantity) = missing
Base.isapprox(::AbstractQuantity, ::Missing; kws...) = missing
Base.isapprox(::Missing, ::AbstractQuantity; kws...) = missing

Base.:(==)(::AbstractQuantity, ::WeakRef) = error("Cannot compare a quantity to a weakref")
Base.:(==)(::WeakRef, ::AbstractQuantity) = error("Cannot compare a weakref to a quantity")

Base.:*(l::AbstractDimensions, r::Number) = error("Please use an `UnionAbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:*(l::Number, r::AbstractDimensions) = error("Please use an `UnionAbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::AbstractDimensions, r::Number) = error("Please use an `UnionAbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::Number, r::AbstractDimensions) = error("Please use an `UnionAbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")

if VERSION < v"1.7"
    for I in (Base.MultiplicativeInverses.UnsignedMultiplicativeInverse, Base.MultiplicativeInverses.SignedMultiplicativeInverse)
        @eval function Base.div(x::T, y::$I{T}, r::RoundingMode=RoundToZero) where {T<:AbstractGenericQuantity}
            return new_quantity(typeof(x), div(ustrip(x), y, r), dimension(x))
        end
    end
end