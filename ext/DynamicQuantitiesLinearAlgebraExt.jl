module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm
import DynamicQuantities: UnionAbstractQuantity, ustrip, dimension, new_quantity

norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

# Define isapprox for vectors of Quantity types
function Base.isapprox(
    u::AbstractArray{Q}, v::AbstractArray{Q};
    atol=new_quantity(Q, zero(T), dimension(first(u))),
    rtol=Base.rtoldefault(T),
    nans::Bool=false, norm::Function=norm
) where {T, D, Q<:UnionAbstractQuantity{T,D}}
    d = norm(u - v)
    if isfinite(d)
        return d <= max(atol, rtol*max(norm(u), norm(v)))
    else
        # Fall back to a component-wise approximate comparison
        return all(ab -> isapprox(uv[1], uv[2]; rtol=rtol, atol=atol, nans=nans), zip(u, v))
    end
end

end
