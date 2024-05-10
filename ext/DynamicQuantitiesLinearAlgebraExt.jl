module DynamicQuantitiesLinearAlgebraExt

using LinearAlgebra: LinearAlgebra as LA
using Compat: allequal
using DynamicQuantities
using DynamicQuantities: DynamicQuantities as DQ, quantity_type, new_quantity, DimensionError
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

    @eval $op(l::$L, r::$R) = DQ.array_op($op, l, r)
end

function Base.:*(
    l::LA.Transpose{Q,<:AbstractVector},
    r::DQ.QuantityArray{T2,1,D,Q,<:AbstractVector{T2}}
) where {
    T2,D<:DQ.AbstractDimensions,Q<:DQ.AbstractRealQuantity{T2,D}
}
    return array_op(Base.:*, l, r)
end


@static if VERSION >= v"1.8.0"
    @eval function LA.svd(A::QuantityArray; full=false, alg::LA.Algorithm=LA.default_svd_alg(ustrip(A)))
        F = LA.svd(ustrip(A), full=full, alg=alg)
        S = QuantityArray(F.S, dimension(A), quantity_type(A))
        return LA.SVD(F.U, S, F.Vt)
    end
    # TODO: functions on SVD type that are working: `size`, `adjoint`, partially working: `inv`, not working: `svdvals`, `ldiv!`.
end


@testitem "svd" begin
    using DynamicQuantities, LinearAlgebra

    if VERSION >= v"1.8.0"
        A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
        QA = QuantityArray(A, u"m/s")

        F = svd(A)
        FQ = svd(QA)

        @test F.U ≈ FQ.U
        @test F.S * u"m/s" ≈ FQ.S
        @test F.Vt ≈ FQ.Vt
        @test size(FQ) == size(F)

        @test FQ.S ≈ [3.0u"m/s", 2.23606797749979u"m/s", 2.0u"m/s", 0.0u"m/s"]

        @test adjoint(FQ).U ≈ adjoint(F).U
        @test adjoint(FQ).S ≈ adjoint(F).S * u"m/s"
        @test adjoint(FQ).Vt ≈ adjoint(F).Vt

        @test QA ≈ FQ.U * Diagonal(FQ.S) * FQ.Vt
    end
end

@static if VERSION >= v"1.8.0"
    @eval function LA.inv(F::LA.SVD{T,Q,TU,TS}) where {T,Q<:UnionAbstractQuantity,TU,TS<:QuantityArray}
        stripped_svd = LA.SVD(F.U, ustrip(F.S), F.Vt)
        return QuantityArray(inv(stripped_svd), inv(dimension(F.S)), quantity_type(F.S))
    end
end

@testitem "inv of svd" begin
    using DynamicQuantities, LinearAlgebra

    if VERSION >= v"1.8.0"
        A = [
            1.0 0.0 0.0
            0.0 2.0 0.0
            0.0 0.0 3.0
        ]
        QA = QuantityArray(A, u"m/s")

        F = svd(A)
        FQ = svd(QA)

        @test inv(FQ) ≈ inv(F) * inv(u"m/s")
        
        # Should be a quantity array for speed:
        @test inv(FQ) isa QuantityArray
    end
end

LA.Diagonal(d::Vector{<:UnionAbstractQuantity}) = LA.Diagonal(QuantityArray(d))

# TODO: See https://github.com/JuliaLang/julia/pull/54440
LA.diagzero(D::LA.Diagonal{T}, _, _) where {T<:Quantity} = zero(first(D))
LA.fzero(S::LA.Diagonal{T}) where {T<:Quantity} = zero(first(S))

@testitem "Diagonal" begin
    using DynamicQuantities, LinearAlgebra

    d = [1.0, 2.0, 3.0]
    QA = Diagonal(QuantityArray(d, u"m"))

    @test QA isa Diagonal{<:UnionAbstractQuantity}
    @test QA.diag isa QuantityArray

    @test QA == Diagonal(d .* u"m")

    QA_true = [
        1.0u"m" 0.0u"m" 0.0u"m"
        0.0u"m" 2.0u"m" 0.0u"m"
        0.0u"m" 0.0u"m" 3.0u"m"
    ]
    
    # This required the `diagzero` call:
    @test QA == QA_true

    # Need fzero for this to work
    @test QA .* ones(3, 3) == QA_true .* ones(3, 3)

    QA2 = Diagonal(d .* u"m")
    @test QA2 isa Diagonal
    @test QA2 == QA_true

    # Throws an error if we pass mismatched elements
    @test_throws DimensionError Diagonal([1.0u"m", 2.0u"s"])

    # With *
    QA3 = Diagonal([1.0u"m/s", 1.0u"m/s", 1.0u"m/s"])
    v = QuantityArray([2.0u"s", 3.0u"s", 4.0u"s"])
    @test QA3 * v == [2.0u"m", 3.0u"m", 4.0u"m"]
end


function LA.diagm(q::QuantityArray)
    return QuantityArray(LA.diagm(ustrip(q)), dimension(q), quantity_type(q))
end
function LA.diagm(m::Integer, n::Integer, q::QuantityArray)
    return QuantityArray(LA.diagm(m, n, ustrip(q)), dimension(q), quantity_type(q))
end
function LA.diagm(q::Vector{Q}) where {Q<:UnionAbstractQuantity}
    allequal(dimension.(q)) || throw(DimensionError(first(q), q))
    return QuantityArray(LA.diagm(ustrip.(q)), dimension(first(q)), Q)
end
function LA.diagm(m::Integer, n::Integer, q::Vector{Q}) where {Q<:UnionAbstractQuantity}
    allequal(dimension.(q)) || throw(DimensionError(first(q), q))
    return QuantityArray(LA.diagm(m, n, ustrip.(q)), dimension(first(q)), Q)
end


@testitem "diagm" begin
    using DynamicQuantities, LinearAlgebra

    A = diagm([1.0, 2.0, 3.0])
    QA1 = diagm([1.0, 2.0, 3.0] .* u"m")
    QA2 = diagm([1.0, 2.0, 3.0]) .* u"m"
    QA3 = [
        1.0u"m" 0.0u"m" 0.0u"m"
        0.0u"m" 2.0u"m" 0.0u"m"
        0.0u"m" 0.0u"m" 3.0u"m"
    ]
    QA4 = diagm(QuantityArray([1.0, 2.0, 3.0], u"m"))
    QA5 = diagm(3, 3, [1.0, 2.0, 3.0] .* u"m")
    QA6 = diagm(3, 3, QuantityArray([1.0, 2.0, 3.0], u"m"))

    @test A == ustrip(QA1)
    @test QA1 == QA2
    @test QA1 == QA3
    @test QA1 == QA4
    @test QA1 == QA5
    @test QA1 == QA6
end

# function LA.eigen(A::QuantityArray; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=LA.eigsortby)
#     F = LA.eigen(ustrip(A), permute=permute, scale=scale, sortby=sortby)
#     return LA.Eigen(QuantityArray(F.values, dimension(A), quantity_type(A)), F.vectors)
# end
## functions available for Eigen objects: eigvals, det. Not implemented: inv, isposdef.

function LA.det(A::QuantityArray)
    return new_quantity(quantity_type(A), LA.det(ustrip(A)), dimension(A)^(size(A,1)))
end


@testitem "det" begin
    using DynamicQuantities, LinearAlgebra

    A = [
        1.0 0.0 0.0
        0.0 2.0 0.0
        0.0 0.0 3.0
    ]
    QA = QuantityArray(A, u"m/s")

    @test det(QA) == 6.0u"m^3/s^3"

    QA2 = diagm([1.0u"m/s", 2.0u"m/s", 3.0u"m/s"])
    @test det(QA2) == 6.0u"m^3/s^3"
end

# TODO: Tests from missing parts of LinearAlgebra interface

end
