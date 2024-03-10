module DynamicQuantitiesLinearAlgebraExt

using DynamicQuantities: UnionAbstractQuantity, QuantityArray, ustrip, dimension, new_quantity

import LinearAlgebra: norm
import DynamicQuantities: _norm

_norm(u::AbstractArray) = norm(u)
norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

end
