module DynamicQuantitiesLinearAlgebraExt

import Base: (*) #, transpose
import LinearAlgebra: norm, inv, (\), svd, Algorithm, default_svd_alg, SVD, Diagonal, Adjoint, Transpose, AbstractRotation, AbstractMatrix, eigen, eigsortby, Eigen, det
import DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity, AbstractDimensions, QuantityArray, Quantity

#export QuantityMatrix

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

# copied from symmetric.jl
#const AdjTransVec = Union{Transpose{<:Any,<:AbstractVector},Adjoint{<:Any,<:AbstractVector}}

# handle ambiguities
*(q::QuantityArray,r::QuantityArray) = QuantityArray(ustrip(q)*ustrip(r),dimension(q)*dimension(r))
*(q::QuantityArray,r::AbstractMatrix) = QuantityArray(ustrip(q)*r,dimension(q))
*(q::QuantityArray,r::AbstractVector)  = QuantityArray(ustrip(q)*r,dimension(q))
*(r::AbstractMatrix,q::QuantityArray) = QuantityArray(r*ustrip(q),dimension(q))
*(r::AbstractVector,q::QuantityArray) = QuantityArray(r*ustrip(q),dimension(q))

# a choice to make this function type stable (to be reviewed/revisited)
#transpose(q::QuantityArray) = QuantityArray(transpose(ustrip(q)),dimension(q))

\(q::QuantityArray,r::QuantityArray) = QuantityArray(ustrip(q)\ustrip(r),dimension(r)/dimension(q))
\(q::QuantityArray,r::Union{AbstractVector,AbstractMatrix}) = QuantityArray(ustrip(q)\r,dimension(q)^-1)
# not implemented, AbstractMatrix \ QuantityArray

inv(Q::QuantityArray) = QuantityArray(inv(ustrip(Q)),dimension(Q)^-1)

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


# """
#     QuantityMatrix{T,N,D<:AbstractDimensions,Q<:AbstractQuantity,V<:AbstractArray}

# A matrix of quantities with value `value` of type `V`, range dimensions `rdimensions` of type `D`, and domain dimensions `ddimensions` of type `D`. Matrices refer to "multipliable matrices" (Multidimensional Analysis, Hart, 1995) which are more restrictive than arrays because units must have a special structure for matrix multiplication to be valid. This is a subtype of `AbstractArray{Q,N}`,
# and so can be used in most places where a normal array would be used, including broadcasting operations.

# # Fields

# - `value`: The underlying array of values. Access with `ustrip(a)`.
# - `rdimensions`: The range dimensions of the matrix. Access with `rdimension(a)`.
# - `ddimensions`: The domain dimensions of the matrix. Access with `ddimension(a)`.

# # Constructors

# - `QuantityMatrix(value::AbstractArray, rdimensions::AbstractDimensions, ddimensions::AbstractDimensions)`: Create a `QuantityMatrix` with value `value` and range dimensions `rdimensions` and domain dimensions `ddimensions`.
# - `QuantityMatrix(value::AbstractArray, rquantity::Vector{Quantity}, dquantity::Vector{Quantity})`: Create a `QuantityMatrix` with value `value` and range dimensions inferred
#    with `dimension(rquantity)` and domain dimensions inferred with `dimension(dquantity)`. This is so that you can easily create an array with the units module, like so:
#    ```julia
#    julia> A = QuantityMatrix(randn(2,2), [1u"m",1u"s"],[1u"kg",1u"s"])
#    ```
# - `QuantityMatrix(v::AbstractArray{<:AbstractQuantity})`: Create a `QuantityMatrix` from an array of quantities, if possible. This means the following
#   syntax works:
#   ```julia
#   julia> A = QuantityMatrix(randn(32) .* 1u"km/s")
#   ```
#   Return `nothing` if the array of quantities does not correspond to the multipliable matrix.
# """
# struct QuantityMatrix{T,N,D<:AbstractDimensions,Q<:AbstractQuantity{T,D},V<:AbstractArray{T,N}} <: AbstractArray{Q,N}
#     value::V
#     rdimensions::D
#     ddimensions::D # ok to force both dimensions to be same type?

#     function QuantityMatrix(v::_V, rd::_D, dd::_D, ::Type{_Q}) where {_T,_N,_D<:AbstractDimensions,_Q<:AbstractQuantity,_V<:AbstractArray{_T,_N}}
#         Q_out = constructor_of(_Q){_T,_D}
#         return new{_T,_N,_D,Q_out,_V}(v, rd, dd)
#     end
# end

# Construct with a Quantity (easier, as you can use the units):
# QuantityArray(v::AbstractArray; kws...) = QuantityArray(v, DEFAULT_DIM_TYPE(; kws...))
# QuantityArray(v::AbstractArray, d::AbstractDimensions) = QuantityArray(v, d, Quantity)
# QuantityArray(v::AbstractArray, q::AbstractQuantity) = QuantityArray(v .* ustrip(q), dimension(q), typeof(q))
# QuantityArray(v::QA) where {Q<:AbstractQuantity,QA<:AbstractArray{Q}} =
#     let
#         allequal(dimension.(v)) || throw(DimensionError(first(v), v))
#         QuantityArray(ustrip.(v), dimension(first(v)), Q)
#     end

#include("matrices.jl")

end
