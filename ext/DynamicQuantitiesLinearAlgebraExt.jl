module DynamicQuantitiesLinearAlgebraExt

import Base: (*)
import LinearAlgebra: norm, inv, (\), svd, Algorithm, default_svd_alg, SVD, Diagonal
import DynamicQuantities: AbstractQuantity, ustrip, dimension, new_quantity, AbstractDimensions, QuantityArray

#export QuantityMatrix

norm(q::AbstractQuantity, p::Real=2) = new_quantity(typeof(q), norm(ustrip(q), p), dimension(q))

*(q::QuantityArray,r::QuantityArray) = QuantityArray(ustrip(q)*ustrip(r),dimension(q)*dimension(r))
*(q::QuantityArray,r::Union{Matrix,Vector}) = QuantityArray(ustrip(q)*r,dimension(q))

\(q::QuantityArray,r::QuantityArray) = QuantityArray(ustrip(q)\ustrip(r),dimension(r)/dimension(q))
    
inv(q::QuantityArray) = QuantityArray(inv(ustrip(q)),dimension(q)^-1)

function svd(A::QuantityArray;full=false,alg::Algorithm = default_svd_alg(ustrip(A))) 
    F = svd(ustrip(A), full=full, alg=alg)
    return SVD(F.U,QuantityArray(F.S,dimension(A)),F.Vt)
end

Diagonal(q::QuantityArray)  = QuantityArray(Diagonal(ustrip(q)),dimension(q))

#    ((length(r) == length(d)) && (length(v) == length(d))) ? UnitfulMatrix(LinearAlgebra.Diagonal(ustrip.(v)),r,d; exact=exact) : error("unit range and domain do not define a square matrix")   

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
