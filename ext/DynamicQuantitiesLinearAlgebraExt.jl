module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm
import DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

end
