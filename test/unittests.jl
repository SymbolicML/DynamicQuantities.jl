using DynamicQuantities
using DynamicQuantities: DEFAULT_DIM_TYPE
using Ratios: SimpleRatio
using SaferIntegers: SafeInt16
using Test

@testset "Basic utilities" begin

    for T in [Float16, Float32, Float64], R in [Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        x = Quantity(T(0.2), R, length=1, mass=2.5)

        @test typeof(x).parameters[1] == T
        @test typeof(x).parameters[2] == R
        @test ulength(x) == R(1 // 1)
        @test umass(x) == R(5 // 2)
        @test ustrip(x) ≈ T(0.2)
        @test dimension(x) == Dimensions(R, length=1, mass=5 // 2)
        if R == DEFAULT_DIM_TYPE
            @test x == Quantity(T(0.2), length=1, mass=2.5)
            @test dimension(x) == Dimensions(length=1, mass=5 // 2)
        end

        y = x^2

        @test typeof(x).parameters[1] == T
        @test typeof(x).parameters[2] == R
        @test ulength(y) == R(2 // 1)
        @test umass(y) == (5 // 1)
        @test ustrip(y) ≈ T(0.04)

        y = x + x

        @test ulength(y) == R(1 // 1)
        @test umass(y) == R(5 // 2)
        @test ustrip(y) ≈ T(0.4)

        if R <: Rational
            @test string(x) == "0.2 𝐋 ¹ 𝐌 ⁵ᐟ²"
            @test string(inv(x)) == "5.0 𝐋 ⁻¹ 𝐌 ⁻⁵ᐟ²"
        end

        @test_throws DimensionError x^2 + x


        y = inv(x)

        @test ulength(y) == R(-1 // 1)
        @test umass(y) == R(-5 // 2)
        @test ustrip(y) ≈ R(5)

        y = x - x

        @test iszero(x) == false
        @test iszero(y) == true
        @test iszero(y.dimensions) == false

        y = x / x

        @test iszero(x.dimensions) == false
        @test iszero(y.dimensions) == true

        y = Quantity(T(2 // 10), R, length=1, mass=5 // 2)

        @test y ≈ x

        y = Quantity(T(2 // 10), R, length=1, mass=6 // 2)

        @test !(y ≈ x)

        y = x * Inf32

        @test typeof(y).parameters[1] == promote_type(T, Float32)
        @test typeof(y).parameters[2] == R
        @test isfinite(x)
        @test !isfinite(y)

        y = x^2.1

        @test typeof(y).parameters[1] == T  # Should not promote! Expect 2.1 to be converted to 21//10
        @test typeof(y).parameters[2] == R
        @test ulength(y) == R(1 * (21 // 10))
        @test umass(y) == R((5 // 2) * (21 // 10))
        @test utime(y) == R(0)
        @test ucurrent(y) == R(0)
        @test utemperature(y) == R(0)
        @test uluminosity(y) == R(0)
        @test uamount(y) == R(0)
        @test ustrip(y) ≈ T(0.2^2.1)
    end

    x = Quantity(-1.2, length=2 // 5)

    @test abs(x) == Quantity(1.2, length=2 // 5)
    @test abs(x) == abs(Quantity(1.2, length=2 // 5))
end

@testset "Fallbacks" begin
    @test ustrip(0.5) == 0.5
    @test dimension(0.5) == Dimensions()
end

@testset "Arrays" begin
    for T in [Float16, Float32, Float64], R in [Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        X = randn(T, 10)
        uX = X .* Dimensions{R}(length=2.5, luminosity=0.5)

        @test eltype(uX) <: Quantity{T,R}
        @test typeof(sum(uX)) <: Quantity{T,R}
        @test sum(X) == ustrip(sum(uX))
        @test dimension(prod(uX)) == prod([Dimensions(length=2.5, luminosity=0.5) for i in 1:10])
        @test dimension(prod(uX)) == prod([Dimensions(R, length=2.5, luminosity=0.5) for i in 1:10])
        @test typeof(dimension(prod(uX))) <: Dimensions{R}
        @test dimension(X[1]) == Dimensions()

        uX = X .* Quantity(2, length=2.5, luminosity=0.5)
        @test sum(X) == 0.5 * ustrip(sum(uX))
    end
end

@testset "Alternate dimension construction" begin
    z = Quantity(-52, length=1) * Dimensions(mass=2)
    z2 = Dimensions(mass=2) * Quantity(-52, length=1)

    @test typeof(z).parameters[1] <: Int
    @test z == z2
    @test ustrip(z) == -52
    @test dimension(z) == Dimensions(length=1, mass=2)
    @test float(z / (z * -1 / 52)) ≈ ustrip(z)

    @test Dimensions(length=1) / 0.5 == Quantity(2.0, length=1)
    @test 0.5 / Dimensions(length=1) == Quantity(0.5, length=-1)
    @test Dimensions(length=1) * 0.5 == Quantity(0.5, length=1)
    @test 0.5 / Quantity(1, length=1) == Quantity(0.5, length=-1)
    @test 0.5 * Quantity(1, length=1) == Quantity(0.5, length=1)
    @test Quantity(0.5) / Dimensions(length=1) == Quantity(0.5, length=-1)
    @test Quantity(0.5, length=2) / Dimensions(length=1) == Quantity(0.5, length=1)
    @test Dimensions(length=1) / Quantity(0.5, length=2, mass=-5) == Quantity(2, length=-1, mass=5)

    @test sqrt(z * -1) == Quantity(sqrt(52), length=1 // 2, mass=1)
    @test cbrt(z) == Quantity(cbrt(-52), length=1 // 3, mass=2 // 3)

    @test 1.0 * (Dimensions(length=3)^2) == Quantity(1.0, length=6)
end

@testset "Manual construction" begin
    d = Dimensions(length=-0.2, luminosity=2)
    q = Quantity(0.5, inv(d))
    @test q == Quantity(0.5, length=0.2, luminosity=-2)
end
