module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm
import DynamicQuantities: UnionAbstractQuantity, QuantityArray, ustrip, dimension, new_quantity

norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

# Define isapprox for vectors of Quantity's
function Base.isapprox(
    u::AbstractArray{Q1}, v::AbstractArray{Q2};
    atol=new_quantity(Q1, zero(T1), dimension(first(u))),
    rtol=Base.rtoldefault(T1),
    nans::Bool=false, norm::Function=norm
) where {T1,D1,Q1<:UnionAbstractQuantity{T1,D1}, T2,D2,Q2<:UnionAbstractQuantity{T2,D2}}
    d = norm(u - v)
    if isfinite(d)
        return d <= max(atol, rtol*max(norm(u), norm(v)))
    else
        # Fall back to a component-wise approximate comparison
        return all(ab -> isapprox(uv[1], uv[2]; rtol=rtol, atol=atol, nans=nans), zip(u, v))
    end
end

# Define isapprox for QuantityArray's
function Base.isapprox(
    u::QuantityArray{T1,Q1}, v::QuantityArray{T2,Q2};
    atol=new_quantity(Q1, zero(T1), dimension(u)),
    rtol=Base.rtoldefault(T1),
    nans::Bool=false, norm::Function=norm
) where {T1,D1,Q1<:UnionAbstractQuantity{T1,D1}, T2,D2,Q2<:UnionAbstractQuantity{T2,D2}}
    d = norm(u - v)
    if isfinite(d)
        return d <= max(atol, rtol*max(norm(u), norm(v)))
    else
        # Fall back to a component-wise approximate comparison
        return all(ab -> isapprox(uv[1], uv[2]; rtol=rtol, atol=atol, nans=nans), zip(u, v))
    end
end

end