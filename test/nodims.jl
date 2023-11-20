using Test
using DynamicQuantities

@testset "NoDims" begin
    @test dimension() isa NoDims
end

@testset "Operators with NoDims" begin
    q1 = Quantity(3.0, NoDims)
    q2 = Quantity(4.0, NoDims)

    @test q1 + q2 == Quantity(7.0, NoDims)
    @test q1 - q2 == Quantity(-1.0, NoDims)
    @test q1 * q2 == Quantity(12.0, NoDims)
    @test q1 / q2 == Quantity(0.75, NoDims)

    @test q1 + 2 == Quantity(5.0, NoDims)
    @test q1 - 2 == Quantity(1.0, NoDims)
    @test q1 * 2 == Quantity(6.0, NoDims)
    @test q1 / 2 == Quantity(1.5, NoDims)

    @test 2 + q1 == Quantity(5.0, NoDims)
    @test 2 - q1 == Quantity(-1.0, NoDims)
    @test 2 * q1 == Quantity(6.0, NoDims)
    @test 2 / q1 == Quantity(2/3, NoDims)

    @test q1 + Inf == Quantity(Inf, NoDims)
    @test q1 - Inf == Quantity(-Inf, NoDims)
    @test q1 * Inf == Quantity(Inf, NoDims)
    @test q1 / Inf == Quantity(0.0, NoDims)

    @test isnan((q1 + NaN).value)
    @test isnan((q1 - NaN).value)
    @test isnan((q1 * NaN).value)
    @test isnan((q1 / NaN).value)
end
