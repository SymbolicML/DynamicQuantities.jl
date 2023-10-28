module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm, svd, Diagonal, eigen, det, diagm

using LinearAlgebra: Algorithm, default_svd_alg, SVD, Adjoint, Transpose, eigsortby, Eigen

using DynamicQuantities: AbstractQuantity, allequal, ustrip, dimension, new_quantity, AbstractDimensions, QuantityArray, Quantity

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

function svd(A::QuantityArray{T,2}; full=false,alg::Algorithm=default_svd_alg(ustrip(A))) where T
    F = svd(ustrip(A), full=full, alg=alg)
    S = [Quantity(F.S[i], dimension(A)) for i in eachindex(F.S)] # julia 1.6 passes but long-winded
    return SVD(F.U, S, F.Vt)
    #Functions on SVD type that are working: `size`, `adjoint`, partially working: `inv`, not working: `svdvals`, `ldiv!`.
    #return SVD(F.U,QuantityArray(F.S,dimension(A)),F.Vt) # julia 1.9 passes with this line but 1.6 fails because 2nd argument is not a Vector
end

Diagonal(q::QuantityArray{T,1}) where T = QuantityArray(Diagonal(ustrip(q)), dimension(q))
function Diagonal(q::Vector{Quantity{T,D}}) where {T,R,D<:AbstractDimensions{R}}
    if allequal(dimension.(q))
        return QuantityArray(Diagonal(ustrip.(q)), dimension(q[begin]))
    else
        error("Diagonal matrix would not be a `QuantityArray`. Vector must contain `Quantity`s with same dimension.")
    end
end

diagm(q::QuantityArray{T,1}) where T = QuantityArray(diagm(ustrip(q)), dimension(q))
diagm(m::Integer, n::Integer, q::QuantityArray{T,1}) where T = QuantityArray(diagm(m,n,ustrip(q)), dimension(q))
function diagm(q::Vector{Quantity{T,D}}) where {T,R,D<:AbstractDimensions{R}}
    if allequal(dimension.(q))
        return QuantityArray(diagm(ustrip.(q)), dimension(q[begin]))
    else
        error("diagm output would not be a `QuantityArray`. Vector must contain `Quantity`s with same dimension.")
    end
end
function diagm(m::Integer, n::Integer, q::Vector{Quantity{T,D}}) where {T,R,D<:AbstractDimensions{R}}
    if allequal(dimension.(q))
        return QuantityArray(diagm(m,n,ustrip.(q)), dimension(q[begin]))
    else
        error("diagm output would not be a `QuantityArray`. Vector must contain `Quantity`s with same dimension.")
    end
end

function eigen(A::QuantityArray{T,2}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) where T
    F = eigen(ustrip(A), permute=permute, scale=scale, sortby=sortby)
    return Eigen(QuantityArray(F.values, dimension(A)), F.vectors)
    # functions available for Eigen objects: eigvals, det. Not implemented: inv, isposdef. 
end

det(A::QuantityArray) = Quantity(det(ustrip(A)), dimension(A)^(size(A,1)))

end
