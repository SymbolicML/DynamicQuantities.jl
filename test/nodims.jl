using Test
using DynamicQuantities: NoDims, dimension, Quantity, UnionAbstractQuantity

@testset "NoDims" begin
    nd = NoDims()
    @test typeof(nd) == NoDims
end

@testset "dimension" begin
    @test dimension(1.0) == NoDims()
    @test dimension(Quantity(1.0, length=1)) != NoDims()
    @test dimension(1.0) == dimension(u"1")
end

@testset "math operations" begin
    q1 = Quantity(1.0, length=1)
    q2 = Quantity(2.0, length=1)
    q3 = Quantity(1.0, length=2)
    nq = 1.0

    @test q1 * q2 == Quantity(2.0, length=2)
    @test_throws DimensionError q1 * q3
    @test q1 * nq == Quantity(1.0, length=1)
    @test nq * q1 == Quantity(1.0, length=1)

    @test q1 / q2 == Quantity(0.5, length=0)
    @test_throws DimensionError q1 / q3
    @test q1 / nq == Quantity(1.0, length=1)
    @test nq / q1 == Quantity(1.0, length=-1)
end
