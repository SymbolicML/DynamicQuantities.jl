module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm, svd, Diagonal, eigen, det, diagm

using LinearAlgebra: Algorithm, default_svd_alg, SVD, Adjoint, eigsortby, Eigen
using DynamicQuantities:
    UnionAbstractQuantity, allequal, ustrip, dimension,
    new_quantity, AbstractDimensions, QuantityArray,
    constructorof, quantity_type

norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

function svd(A::QuantityArray; full=false, alg::Algorithm=default_svd_alg(ustrip(A)))
    F = svd(ustrip(A), full=full, alg=alg)
    Q = quantity_type(A)
    S = [
        constructorof(Q)(F.S[i], dimension(A))
        for i in eachindex(F.S)
    ]
    return SVD(F.U, S, F.Vt)
    #Functions on SVD type that are working: `size`, `adjoint`, partially working: `inv`, not working: `svdvals`, `ldiv!`.
end

Diagonal(q::QuantityArray) = QuantityArray(Diagonal(ustrip(q)), dimension(q), quantity_type(q))
function Diagonal(q::Vector{Q}) where {Q<:UnionAbstractQuantity}
    allequal(dimension.(q)) || throw(DimensionError(first(q), q))
    return QuantityArray(Diagonal(ustrip.(q)), dimension(first(q)), Q)
end

function diagm(q::QuantityArray)
    return QuantityArray(diagm(ustrip(q)), dimension(q), quantity_type(q))
end
function diagm(m::Integer, n::Integer, q::QuantityArray)
    return QuantityArray(diagm(m, n, ustrip(q)), dimension(q), quantity_type(q))
end
function diagm(q::Vector{Q}) where {Q<:UnionAbstractQuantity}
    allequal(dimension.(q)) || throw(DimensionError(first(q), q))
    return QuantityArray(diagm(ustrip.(q)), dimension(first(q)), Q)
end
function diagm(m::Integer, n::Integer, q::Vector{Q}) where {Q<:UnionAbstractQuantity}
    allequal(dimension.(q)) || throw(DimensionError(first(q), q))
    return QuantityArray(diagm(m,n,ustrip.(q)), dimension(first(q)), Q)
end

function eigen(A::QuantityArray; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby)
    F = eigen(ustrip(A), permute=permute, scale=scale, sortby=sortby)
    return Eigen(QuantityArray(F.values, dimension(A), quantity_type(A)), F.vectors)
end
# functions available for Eigen objects: eigvals, det. Not implemented: inv, isposdef.

det(A::QuantityArray) = constructorof(quantity_type(A))(det(ustrip(A)), dimension(A)^(size(A,1)))

end
