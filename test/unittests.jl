using DynamicQuantities
using DynamicQuantities: FixedRational
using DynamicQuantities: DEFAULT_DIM_BASE_TYPE, DEFAULT_DIM_TYPE, DEFAULT_VALUE_TYPE
using Ratios: SimpleRatio
using SaferIntegers: SafeInt16
using LinearAlgebra: norm
using Test

@testset "Basic utilities" begin

    for T in [DEFAULT_VALUE_TYPE, Float16, Float32, Float64], R in [DEFAULT_DIM_BASE_TYPE, Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        D = Dimensions{R}
        x = Quantity(T(0.2), D, length=1, mass=2.5)

        @test typeof(x).parameters[1] == T
        @test typeof(x).parameters[2] == D
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
        @test typeof(x).parameters[2] == D
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

        y = Quantity(T(2 // 10), D, length=1, mass=5 // 2)

        @test y ≈ x

        y = Quantity(T(2 // 10), D, length=1, mass=6 // 2)

        @test !(y ≈ x)

        y = x * Inf32

        @test typeof(y).parameters[1] == promote_type(T, Float32)
        @test typeof(y).parameters[2] == D
        @test isfinite(x)
        @test !isfinite(y)

        y = x^2.1

        @test typeof(y).parameters[1] == T  # Should not promote! Expect 2.1 to be converted to 21//10
        @test typeof(y).parameters[2] == D
        @test ulength(y) == R(1 * (21 // 10))
        @test umass(y) == R((5 // 2) * (21 // 10))
        @test utime(y) == R(0)
        @test ucurrent(y) == R(0)
        @test utemperature(y) == R(0)
        @test uluminosity(y) == R(0)
        @test uamount(y) == R(0)
        @test ustrip(y) ≈ T(0.2^2.1)

        dimensionless = Quantity(one(T), D)
        y = T(2) + dimensionless
        @test ustrip(y) == T(3)
        @test dimension(y) == Dimensions(R)
        @test typeof(y) == Quantity{T,D}

        y = T(2) - dimensionless
        @test ustrip(y) == T(1)
        @test dimension(y) == Dimensions(R)
        @test typeof(y) == Quantity{T,D}

        y = dimensionless + T(2)
        @test ustrip(y) == T(3)
        y = dimensionless - T(2)
        @test ustrip(y) == T(-1)

        @test_throws DimensionError Quantity(one(T), D,  length=1) + 1.0
        @test_throws DimensionError Quantity(one(T), D, length=1) - 1.0
        @test_throws DimensionError 1.0 + Quantity(one(T), D, length=1)
        @test_throws DimensionError 1.0 - Quantity(one(T), D, length=1)
    end

    x = Quantity(-1.2, length=2 // 5)

    @test typemax(x) == Quantity(typemax(-1.2), length=2 // 5)
    @test typemax(typeof(x)) == Quantity(typemax(typeof(-1.2)))

    @test abs(x) == Quantity(1.2, length=2 // 5)
    @test abs(x) == abs(Quantity(1.2, length=2 // 5))
    @test abs2(x) == Quantity(abs2(-1.2), length=4 // 5)

    @test iszero(x) == false
    @test iszero(x * 0) == true
    @test isfinite(x) == true
    @test isfinite(x * Inf) == false
    @test isfinite(x * NaN) == false
    @test isinf(x * Inf) == true
    @test isnan(x) == false
    @test isnan(x * NaN) == true

    @test nextfloat(x) == Quantity(nextfloat(-1.2), length=2 // 5)
    @test prevfloat(x) == Quantity(prevfloat(-1.2), length=2 // 5)

    y = Quantity(-1, mass=1)
    @test unsigned(y) == Quantity(unsigned(-1), mass=1)
end

@testset "Complex numbers" begin
    x = (0.5 + 0.6im) * u"km/s"
    @test string(x) == "(500.0 + 600.0im) m s⁻¹"
    @test typeof(x) == Quantity{Complex{Float64}, DEFAULT_DIM_TYPE}
    @test typeof(x^2) == Quantity{Complex{Float64}, DEFAULT_DIM_TYPE}
    @test x^2/u"km/s"^2 ≈ (0.5 + 0.6im)^2
    @test x^2.5 ≈ (-9.896195997465055e6 + 1.38810912834778e7im) * u"m^(5/2) * s^(-5/2)"
    @test isreal(x) == false
    @test isreal(abs2(x)) == true
    @test real(x) == 0.5 * u"km/s"
    @test imag(x) == 0.6 * u"km/s"
    @test conj(x) == (0.5 - 0.6im) * u"km/s"
    @test adjoint(ustrip(x^2)) ≈ adjoint(x^2) / u"m/s"^2
end

@testset "Fallbacks" begin
    @test ustrip(0.5) == 0.5
    @test ustrip(ones(32)) == ones(32)
    @test dimension(Dimensions()) === Dimensions()

    @test_throws MethodError Dimensions(1.0)
    @test_throws ErrorException ustrip(Dimensions())
end

@testset "Arrays" begin
    for T in [Float16, Float32, Float64], R in [Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        D = Dimensions{R}

        X = randn(T, 10)
        uX = X .* Quantity{T,D}(1, length=2.5, luminosity=0.5)

        @test eltype(uX) <: Quantity{T,D}
        @test typeof(sum(uX)) <: Quantity{T,D}
        @test sum(X) == ustrip(sum(uX))
        @test dimension(prod(uX)) == dimension(prod([Quantity(T(1), D, length=2.5, luminosity=0.5) for i in 1:10]))
        @test dimension(prod(uX)) == dimension(prod([Quantity(T(1), D, length=2.5, luminosity=0.5) for i in 1:10]))
        @test typeof(dimension(prod(uX))) <: D

        uX = X .* Quantity(2, length=2.5, luminosity=0.5)
        @test sum(X) == 0.5 * ustrip(sum(uX))

        x = Quantity(ones(T, 32))
        @test ustrip(x + ones(T, 32))[32] == 2
        @test typeof(x + ones(T, 32)) <: Quantity{Vector{T}}
        @test typeof(x - ones(T, 32)) <: Quantity{Vector{T}}
        @test typeof(ones(T, 32) * Quantity(T(1), D, length=1)) <: Quantity{Vector{T}}
        @test typeof(ones(T, 32) / Quantity(T(1), D, length=1)) <: Quantity{Vector{T}}
        @test ones(T, 32) / Quantity(T(1), length=1) == Quantity(ones(T, 32), length=-1)
    end

    x = randn(32) .* u"km/s"
    @test ustrip(x) == ustrip.(x)
    @test dimension(x) == dimension(u"km/s")
    @test_throws DimensionError dimension([u"km/s", u"km"])

    @test norm(x, 2) ≈ norm(ustrip.(x), 2) * u"m/s"
    @test norm(Quantity(ustrip(x), length=1, time=-1), 2) ≈ norm(ustrip.(x), 2) * u"m/s"

    @test ustrip(x') == ustrip(x)'
end

@testset "Alternate dimension construction" begin
    z = Quantity(-52, length=1) * Dimensions(mass=2)
    z2 = Dimensions(mass=2) * Quantity(-52, length=1)

    @test typeof(z).parameters[1] <: Int
    @test z == z2
    @test ustrip(z) == -52
    @test dimension(z) == Dimensions(length=1, mass=2)
    @test float(z / (z * -1 / 52)) ≈ ustrip(z)

    # Invalid ways to make a quantity:
    @test_throws ErrorException Dimensions(length=1) / 0.5 == Quantity(2.0, length=1)
    @test_throws ErrorException 0.5 / Dimensions(length=1)
    @test_throws ErrorException Dimensions(length=1) * 0.5

    @test 0.5 / Quantity(1, length=1) == Quantity(0.5, length=-1)
    @test 0.5 * Quantity(1, length=1) == Quantity(0.5, length=1)
    @test Quantity(0.5) / Dimensions(length=1) == Quantity(0.5, length=-1)
    @test Quantity(0.5, length=2) / Dimensions(length=1) == Quantity(0.5, length=1)
    @test Dimensions(length=1) / Quantity(0.5, length=2, mass=-5) == Quantity(2, length=-1, mass=5)

    @test Dimensions{Int8}(zeros(Int, 7)...) == Dimensions{Int8}()

    @test zero(Quantity(0.0+0.0im)) + Quantity(1) == Quantity(1.0+0.0im, length=Int8(0))
    @test oneunit(Quantity(0.0+0.0im)) - Quantity(1) == Quantity(0.0+0.0im, length=Int8(0))
    @test typeof(one(Dimensions{Int16})) == Dimensions{Int16}
    @test one(Dimensions{Int16}) == Dimensions(mass=Int16(0))

    @test zero(Quantity(0.0im)) == Quantity(0.0+0.0im)
    @test one(Quantity{ComplexF64}) == Quantity(1.0+0.0im)

    @test zero(Quantity(0.0)) == Quantity(0.0)
    @test typeof(zero(Quantity(0.0))) == Quantity{Float64,DEFAULT_DIM_TYPE}
    @test oneunit(Quantity(1.0)) - Quantity(1.0) == Quantity(0.0)
    @test typeof(one(Quantity(1.0))) == Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
    @test one(Dimensions) == Dimensions()
    @test one(Dimensions()) == Dimensions()
    @test typeof(one(Quantity)) == Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
    @test ustrip(one(Quantity)) === one(DEFAULT_VALUE_TYPE)
    @test typeof(one(Quantity(ones(32, 32)))) == Quantity{Matrix{Float64},DEFAULT_DIM_TYPE}
    @test dimension(one(Quantity(ones(32, 32), length=1))) == Dimensions()

    x = Quantity(1, length=1)

    @test zero(x) == Quantity(0, length=1)
    @test typeof(zero(x)) == Quantity{Int64,DEFAULT_DIM_TYPE}
    
    # Invalid calls:
    @test_throws ErrorException zero(Quantity)
    @test_throws ErrorException zero(Dimensions())
    @test_throws ErrorException zero(Dimensions)
    @test_throws ErrorException oneunit(Quantity)
    @test_throws ErrorException oneunit(Dimensions())
    @test_throws ErrorException oneunit(Dimensions)

    @test sqrt(z * -1) == Quantity(sqrt(52), length=1 // 2, mass=1)
    @test cbrt(z) == Quantity(cbrt(-52), length=1 // 3, mass=2 // 3)

    @test_throws ErrorException 1.0 * (Dimensions(length=3)^2)

    x = 0.9u"km/s"
    y = 0.3 * x
    @test x > y
    @test y < x

    x = Quantity(1.0)

    @test x == 1.0
    @test x >= 1.0
    @test x < 2.0

    @test_throws DimensionError x < 1.0u"m"
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
    q32_32 = convert(Quantity{Float32,Dimensions{Rational{Int32}}}, q)
    @test typeof(q) == Quantity{Float64,Dimensions{Rational{Int16}}}
    @test typeof(q32_32) == Quantity{Float32,Dimensions{Rational{Int32}}}
    @test ustrip(q) == 0.5
    @test ustrip(q32_32) == 0.5
    @test typeof(ustrip(q)) == Float64
    @test typeof(ustrip(q32_32)) == Float32
    @test dimension(q32_32) == dimension(q)
    @test umass(q) == 2
    @test umass(q32_32) == 2
    @test typeof(umass(q32_32)) == Rational{Int32}
    @test typeof(convert(Quantity{Float16}, q)) == Quantity{Float16,Dimensions{Rational{Int16}}}
    @test convert(Quantity, q) === q

    # Test that regular type promotion applies:
    q = Quantity(2, d)
    @test typeof(q) == Quantity{Int64,typeof(d)}
    @test typeof(q ^ 2) == Quantity{Int64,typeof(d)}
    @test typeof(0.5 * q) == Quantity{Float64,typeof(d)}
    @test typeof(inv(q)) == Quantity{Float64,typeof(d)}

    # Automatic conversions via constructor:
    for T in [Float16, Float32, Float64, BigFloat], R in [DEFAULT_DIM_BASE_TYPE, Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        D = Dimensions{R}
        q = Quantity{T,D}(2, length=1.5)
        @test typeof(q) == Quantity{T,D}
        @test typeof(ustrip(q)) == T
        @test typeof(ulength(q)) == R

        # Now, without R, the default will be DEFAULT_DIM_BASE_TYPE:
        q = Quantity{T}(2, length=1.5)
        @test typeof(q) == Quantity{T,DEFAULT_DIM_TYPE}
        @test typeof(ustrip(q)) == T
        @test typeof(ulength(q)) == DEFAULT_DIM_BASE_TYPE

        # Just dimensions:
        d = D(length=1.5)
        @test typeof(d) == D
        @test typeof(ulength(d)) == R
    end
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

    y32 = convert(Quantity{Float32,Dimensions{Rational{Int16}}}, y)
    @test typeof(y32) == Quantity{Float32,Dimensions{Rational{Int16}}}
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

@testset "Constants" begin
    @test Constants.h * Constants.c / (1000.0u"nm") ≈ 1.9864458571489284e-19u"J"

    # Compute period of Earth based on solar mass and semi-major axis:
    a = u"Constants.au"
    @test isapprox(sqrt(4π^2 * a^3 / (Constants.G * Constants.M_sun)), 1u"yr"; rtol=1e-3)
end

@testset "Additional tests of FixedRational" begin
    @test convert(Int64, FixedRational{Int64,1000}(2 // 1)) == 2
    @test convert(Int32, FixedRational{Int64,1000}(3 // 1)) == 3
end

struct MyDimensions{R} <: AbstractDimensions{R}
    length::R
    mass::R
    time::R
end
struct MyQuantity{T,D<:AbstractDimensions} <: AbstractQuantity{T,D}
    value::T
    dimensions::D
end

@testset "Custom dimensions" begin
    for T in [Float32, Float64], R in [Rational{Int64}, Rational{Int32}]
        D = MyDimensions{R}
        x = MyQuantity(T(0.1), D, length=0.5)
        @test x * x ≈ MyQuantity(T(0.01), D, length=1)
        @test typeof(x * x) == MyQuantity{T,D}
        @test one(MyQuantity{T,D}) == MyQuantity(one(T), MyDimensions(R))
        @test zero(x) == MyQuantity(zero(T), D, length=0.5)
        @test oneunit(x) + x == MyQuantity(T(1.1), D, length=0.5)
        @test typeof(oneunit(x) + x) == MyQuantity{T,D}

        # Automatic conversions:
        @test typeof(MyQuantity(1, MyDimensions, length=0.5)) == MyQuantity{typeof(1),MyDimensions{DEFAULT_DIM_BASE_TYPE}}
        @test typeof(MyQuantity{T}(1, MyDimensions, length=0.5)) == MyQuantity{T,MyDimensions{DEFAULT_DIM_BASE_TYPE}}
        @test typeof(MyQuantity{T}(1, D, length=0.5)) == MyQuantity{T,D}
        @test typeof(MyQuantity{T,D}(0.1, length=0.5)) == MyQuantity{T,D}
        @test typeof(0.5 * MyQuantity{T,D}(0.1, length=0.5)) == MyQuantity{promote_type(T,Float64),D}

        x = MyQuantity(big(0.1), length=1)
        @test typeof(x) == MyQuantity{BigFloat,DEFAULT_DIM_TYPE}
        @test typeof(MyQuantity{T}(x)) == MyQuantity{T,DEFAULT_DIM_TYPE}
        @test typeof(MyQuantity{T,D}(x)) == MyQuantity{T,D}

        # Using MyDimensions inside regular Quantity:
        x = Quantity(T(0.1), MyDimensions(R, length=0.5))
        @test typeof(x) == Quantity{T,MyDimensions{R}}
        @test typeof(x * x) == Quantity{T,MyDimensions{R}}
        @test ulength(x * x) == 1
        @test dimension(x * x) == MyDimensions(R, length=1)

        # Errors:
        @test_throws ErrorException zero(MyQuantity{T,D})
        @test_throws ErrorException oneunit(MyQuantity{T,D})
        @test_throws ErrorException 1.0 * MyDimensions()
    end
    @test MyQuantity(0.1, DEFAULT_DIM_TYPE, length=0.5) == MyQuantity(0.1, length=0.5)
    @test MyQuantity(0.1, DEFAULT_DIM_TYPE, length=0.5) == MyQuantity(0.1, length=1//2)

    # Can construct using args directly:
    @test typeof(MyDimensions(1, 1, 1)) == MyDimensions{Int}
    @test typeof(MyDimensions{Float64}(1, 1, 1)) == MyDimensions{Float64}

    # But, we always need to use a quantity when mixing with mathematical operations:
    @test_throws ErrorException MyQuantity(0.1) + 0.1 * MyDimensions()
end

@testset "Symbolic dimensions" begin
    q = 1.5us"km/s"
    @test q == 1.5 * us"km" / us"s"
    @test typeof(q) <: Quantity{Float64,<:SymbolicDimensions}
    @test string(dimension(q)) == "s⁻¹ km"
    @test expand_units(q) == 1.5u"km/s"
    @test string(dimension(us"Constants.au^1.5")) == "au³ᐟ²"
    @test string(dimension(expand_units(us"Constants.au^1.5"))) == "m³ᐟ²"
    @test expand_units(2.3us"Constants.au^1.5") ≈ 2.3u"Constants.au^1.5"
    @test iszero(dimension(us"1.0")) == true
    @test expand_units(inv(us"Constants.au")) ≈ 1/u"Constants.au"
    @test dimension(inv(us"s") * us"km") == dimension(us"km/s")
    @test dimension(inv(us"s") * us"m") != dimension(us"km/s")
    @test dimension(expand_units(inv(us"s") * us"m")) == dimension(expand_units(us"km/s"))

    @test_throws ErrorException sym_uparse("'c'")

    # For constants which have a namespace collision, the numerical expansion is used:
    @test dimension(us"Constants.au")[:au] == 1
    @test dimension(us"Constants.h")[:h] == 0
    @test dimension(us"h")[:h] == 1

    @test us"Constants.h" != us"h"
    @test expand_units(us"Constants.h") == u"Constants.h"

    # Actually expands to:
    @test dimension(us"Constants.h")[:m] == 2
    @test dimension(us"Constants.h")[:s] == -1
    @test dimension(us"Constants.h")[:kg] == 1

    # So the numerical value is different from other constants:
    @test ustrip(us"Constants.h") == ustrip(u"Constants.h")
    @test ustrip(us"Constants.au") != ustrip(u"Constants.au")
end

@testset "Test ambiguities" begin
    R = DEFAULT_DIM_BASE_TYPE
    x = convert(R, 10)
    y = convert(R, 5)
    @test promote(x, y) == (x, y)
    @test_throws AssertionError promote(x, convert(FixedRational{Int32,100}, 10))
    @test round(Missing, x) === missing

    x = 1.0u"m"
    y = missing
    @test isless(x, y) === missing
    @test isless(y, x) === missing
    @test (x == y) === missing
    @test (y == x) === missing
    @test isapprox(x, y) === missing
    @test isapprox(y, x) === missing

    x = 1.0u"m"
    s = "test"
    y = WeakRef(s)
    @test_throws ErrorException x == y
    @test_throws ErrorException y == x
end
