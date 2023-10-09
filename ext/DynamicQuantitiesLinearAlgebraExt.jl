module DynamicQuantitiesLinearAlgebraExt

if isdefined(Base, :get_extension)
    import LinearAlgebra: norm
    import DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity
else
    import ..LinearAlgebra: norm
    import ..DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity
end

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

end
