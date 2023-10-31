module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm
import DynamicQuantities: UnionAbstractQuantity, ustrip, dimension, new_quantity

norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

end
