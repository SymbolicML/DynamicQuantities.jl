module DynamicQuantitiesLinearAlgebraExt

using DynamicQuantities: UnionAbstractQuantity, QuantityArray, ustrip, dimension, new_quantity, array_op

import LinearAlgebra: svd, Diagonal, eigen, det, diagm

using LinearAlgebra: LinearAlgebra as LA, Algorithm, default_svd_alg, SVD, Adjoint, eigsortby, Eigen
using DynamicQuantities:
    DynamicQuantities as DQ, UnionAbstractQuantity, allequal, ustrip, dimension,
    new_quantity, AbstractDimensions, QuantityArray,
    constructorof, quantity_type
using TestItems: @testitem

DQ.is_ext_loaded(::Val{:LinearAlgebra}) = true
DQ.norm(u) = LA.norm(u)
LA.norm(q::UnionAbstractQuantity, p::Real=2) = new_quantity(typeof(q), LA.norm(ustrip(q), p), dimension(q))

# Deal with ambiguous array operations:
for op in (:(Base.:*), :(Base.:/), :(Base.:\)),
    Q_ARRAY_TYPE in (:(QuantityArray{<:Any,1}), :(QuantityArray{<:Any,2})),
    ARRAY_TYPE in (
        LA.Transpose{<:Any, <:AbstractVector},
        LA.Transpose{<:Any, <:AbstractMatrix},
        LA.Transpose{<:Any, <:LA.Bidiagonal},
        LA.Adjoint{<:Any, <:AbstractVector},
        LA.Adjoint{<:Any, <:AbstractMatrix},
        LA.Adjoint{<:Any, <:LA.Bidiagonal},
        LA.Adjoint{<:Number, <:AbstractVector},
        Union{LA.Transpose{T,V}, LA.Adjoint{T,V}} where {T,V<:AbstractVector},
        Union{LA.Transpose{T,V}, LA.Adjoint{T,V}} where {T,V<:LA.Bidiagonal},
        LA.AbstractTriangular,
        Union{LA.LowerTriangular,LA.UpperTriangular},
        Union{LA.UnitLowerTriangular,LA.UnitUpperTriangular},
        LA.Diagonal,
        LA.Bidiagonal,
        LA.SymTridiagonal,
        Union{LA.Hermitian{T,S}, LA.Symmetric{T,S}} where {T,S},
    ),
    (L, R) in ((Q_ARRAY_TYPE, ARRAY_TYPE), (ARRAY_TYPE, Q_ARRAY_TYPE))

    @eval $op(l::$L, r::$R) = array_op($op, l, r)
end

function Base.:*(
    l::LA.Transpose{Q,<:AbstractVector},
    r::DQ.QuantityArray{T2,1,D,Q,<:AbstractVector{T2}}
) where {
    T2,D<:DQ.AbstractDimensions,Q<:DQ.AbstractRealQuantity{T2,D}
}
    return array_op(Base.:*, l, r)
end


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
