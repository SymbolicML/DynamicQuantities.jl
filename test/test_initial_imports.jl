using DynamicQuantities: DynamicQuantity as DQ
using Test

@test !DQ.is_ext_loaded(Val(:LinearAlgebra))
a = QuantityArray([1.0], u"m")

@test_throws ErrorException isapprox(a, a)
@test_throws "LinearAlgebra" isapprox(a, a)

