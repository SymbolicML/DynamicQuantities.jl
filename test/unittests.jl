using DynamicQuantities
using DynamicQuantities: DEFAULT_DIM_TYPE, DEFAULT_VALUE_TYPE, DIMENSION_NAMES
using Ratios: SimpleRatio
using SaferIntegers: SafeInt16
using Test

@testset "Basic utilities" begin

    for T in [DEFAULT_VALUE_TYPE, Float16, Float32, Float64], R in [DEFAULT_DIM_TYPE, Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
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
            @test string(x) == "0.2 m kg⁵ᐟ²"
            @test string(inv(x)) == "5.0 m⁻¹ kg⁻⁵ᐟ²"
        end

        @test_throws DimensionError x^2 + x

        # Check output of error:
        try
            x^2 + x
            @test false
        catch e
            r"DimensionError: .* and .* have incompatible dimensions."
            io = IOBuffer()
            showerror(io, e)
            msg = String(take!(io))
            @test occursin("DimensionError", msg)
            @test occursin("incompatible dimensions", msg)
        end

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

    @test Dimensions{Int8}([0 for i=1:length(DIMENSION_NAMES)]...) == Dimensions{Int8}()

    @test zero(Quantity{ComplexF64,Int8}) + Quantity(1) == Quantity(1.0+0.0im, length=Int8(0))
    @test one(Quantity{ComplexF64,Int8}) - Quantity(1) == Quantity(0.0+0.0im, length=Int8(0))
    @test typeof(one(Dimensions{Int16})) == Dimensions{Int16}
    @test one(Dimensions{Int16}) == Dimensions(mass=Int16(0))

    @test zero(Quantity{ComplexF64}) == Quantity(0.0+0.0im)
    @test one(Quantity{ComplexF64}) == Quantity(1.0+0.0im)

    @test zero(Quantity) == Quantity(0.0)
    @test typeof(zero(Quantity)) == Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
    @test one(Quantity) - Quantity(1) == Quantity(0.0)
    @test typeof(one(Quantity)) == Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
    @test typeof(one(Dimensions)) == Dimensions{DEFAULT_DIM_TYPE}
    @test one(Dimensions) == Dimensions()

    @test sqrt(z * -1) == Quantity(sqrt(52), length=1 // 2, mass=1)
    @test cbrt(z) == Quantity(cbrt(-52), length=1 // 3, mass=2 // 3)

    @test 1.0 * (Dimensions(length=3)^2) == Quantity(1.0, length=6)
end

@testset "Manual construction" begin
    d = Dimensions(length=-0.2, luminosity=2)
    q = Quantity(0.5, inv(d))
    @test q == Quantity(0.5, length=0.2, luminosity=-2)
end

@testset "Conversions" begin
    d = Dimensions(Rational{Int16}, mass=2)
    d32 = convert(Dimensions{Rational{Int32}}, d)
    @test typeof(d) == Dimensions{Rational{Int16}}
    @test typeof(d32) == Dimensions{Rational{Int32}}
    @test umass(d) == 2
    @test umass(d32) == 2
    @test typeof(umass(d32)) == Rational{Int32}

    # Should not change:
    @test convert(Dimensions, d) === d

    q = Quantity(0.5, d)
    q32_32 = convert(Quantity{Float32,Rational{Int32}}, q)
    @test typeof(q) == Quantity{Float64,Rational{Int16}}
    @test typeof(q32_32) == Quantity{Float32,Rational{Int32}}
    @test ustrip(q) == 0.5
    @test ustrip(q32_32) == 0.5
    @test typeof(ustrip(q)) == Float64
    @test typeof(ustrip(q32_32)) == Float32
    @test dimension(q32_32) == dimension(q)
    @test umass(q) == 2
    @test umass(q32_32) == 2
    @test typeof(umass(q32_32)) == Rational{Int32}
    @test typeof(convert(Quantity{Float16}, q)) == Quantity{Float16,Rational{Int16}}
    @test convert(Quantity, q) === q
end

@testset "Units" begin
    x = 1.3u"km/s^2"
    @test ustrip(x) == 1300  # SI base units
    @test ulength(x) == 1
    @test utime(x) == -2

    y = 0.9u"sqrt(mΩ)"
    @test typeof(y) == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test ustrip(y) ≈ 0.02846049894151541
    @test ucurrent(y) == -1
    @test ulength(y) == 1

    y = BigFloat(0.3) * u"mΩ"
    @test typeof(y) == Quantity{BigFloat,DEFAULT_DIM_TYPE}
    @test ustrip(y) ≈ 0.0003
    @test ulength(y) == 2

    y32 = convert(Quantity{Float32,Rational{Int16}}, y)
    @test typeof(y32) == Quantity{Float32,Rational{Int16}}
    @test ustrip(y32) ≈ 0.0003

    z = u"yr"
    @test utime(z) == 1
    @test ustrip(z) ≈ 60 * 60 * 24 * 365.25

    # Test type stability of extreme range of units
    @test typeof(u"1") == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"1f0") == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"s"^2) == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"Ω") == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"Gyr") == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"fm") == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test typeof(u"fm"^2) == Quantity{Float64,DEFAULT_DIM_TYPE}

    @test_throws LoadError eval(:(u":x"))
end
