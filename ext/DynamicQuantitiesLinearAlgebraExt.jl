module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm
import DynamicQuantities: AbstractUnionQuantity, ustrip, dimension, new_quantity

norm(q::AbstractUnionQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

end
