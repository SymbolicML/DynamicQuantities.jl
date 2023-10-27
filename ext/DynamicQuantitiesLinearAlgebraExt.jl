module DynamicQuantitiesLinearAlgebraExt

import LinearAlgebra: norm, inv, (\), svd, Algorithm, default_svd_alg, SVD, Diagonal, Adjoint, Transpose, AbstractRotation, AbstractMatrix, eigen, eigsortby, Eigen, det
import DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity, AbstractDimensions, QuantityArray, Quantity

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

\(q::QuantityArray,r::QuantityArray) = QuantityArray(ustrip(q)\ustrip(r),dimension(r)/dimension(q))
\(q::QuantityArray,r::Union{AbstractVector,AbstractMatrix}) = QuantityArray(ustrip(q)\r,inv(dimension(q)))
# not implemented, AbstractMatrix \ QuantityArray

inv(Q::QuantityArray) = QuantityArray(inv(ustrip(Q)),inv(dimension(Q)))

"""
    svd(A::QuantityArray; full::Bool = false, alg::Algorithm = default_svd_alg(A)) -> SVD

    Singular value decomposition (SVD) of `QuantityArray`.
    Exists for uniform matrices which includes all `QuantityArray`s (pp. 124, Hart, 1995).

    Returns SVD factorization of parametric type: `SVD{T, Quantity{T, Dimensions{DynamicQuantities.FixedRational{Int32, 25200}}}, Matrix{T}, QuantityArray{T, 1, Dimensions{DynamicQuantities.FixedRational{Int32, 25200}}, Quantity{T, Dimensions{DynamicQuantities.FixedRational{Int32, 25200}}}, Vector{T}}}`.

    Factorization `F` can be deconstructed: `U,σ,V = F`. 

    Functions working: , `size`, `adjoint`.
    Functions partially working: `inv`
    Functions not working: `svdvals`, `ldiv!`.
"""
function svd(A::QuantityArray;full=false,alg::Algorithm = default_svd_alg(ustrip(A))) 
    F = svd(ustrip(A), full=full, alg=alg)
    return SVD(F.U,QuantityArray(F.S,dimension(A)),F.Vt)
end

Diagonal(q::QuantityArray)  = QuantityArray(Diagonal(ustrip(q)),dimension(q))

"""
    function eigen(A::T;permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) where T <: AbstractMultipliableMatrix

    Thin wrapper for `eigen` with same keyword arguments as `LinearAlgebra.eigen`.
    Only squarable matrices have eigenstructure (pp. 96, Hart, 1995).
    Eigenvalues have the same dimensions as A[1,1].
    Eigenvectors are parallel to the domain and range.
    There are multiple ways to distribute the units among the eigenvectors, however.
    If 𝐀 is endomorphic (i.e., the dimensional domain and range are the same), then the dimensional domain should
    be taken as the units of the eigenvectors (pp. 205, Hart, 1995).  
    In the general case, physical intuition and the equation 𝐀𝐱 = λ𝐱
    dictate that the units of the eigenvectors are equal to the dimensional domain of 𝐀 (pp. 206, Hart, 1995).

    The following functions are available for `Eigen(::QuantityArray)` objects: eigvals, [`det`](@ref).  
    Functions not working: [`inv`](@ref) and [`isposdef`](@ref).
"""
function eigen(A::QuantityArray;permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) 
    F = eigen(ustrip(A), permute=permute, scale=scale, sortby=sortby)
    return Eigen(QuantityArray(F.values,dimension(A)), F.vectors)
end

det(A::QuantityArray) = Quantity(det(ustrip(A)),dimension(A)^(size(A,1)))

end
