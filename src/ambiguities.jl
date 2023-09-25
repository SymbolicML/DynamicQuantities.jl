Base.isless(::AbstractQuantity, ::Missing) = missing
Base.isless(::Missing, ::AbstractQuantity) = missing
Base.:(==)(::AbstractQuantity, ::Missing) = missing
Base.:(==)(::Missing, ::AbstractQuantity) = missing
Base.isapprox(::AbstractQuantity, ::Missing; kws...) = missing
Base.isapprox(::Missing, ::AbstractQuantity; kws...) = missing

Base.:(==)(::AbstractQuantity, ::WeakRef) = error("Cannot compare a quantity to a weakref")
Base.:(==)(::WeakRef, ::AbstractQuantity) = error("Cannot compare a weakref to a quantity")
