using DynamicQuantities
using DynamicQuantities: FixedRational
using DynamicQuantities: DEFAULT_DIM_BASE_TYPE, DEFAULT_DIM_TYPE, DEFAULT_VALUE_TYPE
using DynamicQuantities: array_type, value_type, dim_type, quantity_type
using DynamicQuantities: GenericQuantity
using Ratios: SimpleRatio
using SaferIntegers: SafeInt16
using StaticArrays: SArray, MArray
using LinearAlgebra: norm
using Test

function record_show(s, f=show)
    io = IOBuffer()
    f(io, s)
    return String(take!(io))
end

@testset "Basic utilities" begin

    for Q in [Quantity, GenericQuantity], T in [DEFAULT_VALUE_TYPE, Float16, Float32, Float64], R in [DEFAULT_DIM_BASE_TYPE, Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
        D = Dimensions{R}
        x = Q(T(0.2), D, length=1, mass=2.5)

        @test typeof(x).parameters[1] == T
        @test typeof(x).parameters[2] == D
        @test ulength(x) == R(1 // 1)
        @test umass(x) == R(5 // 2)
        @test ustrip(x) ≈ T(0.2)
        @test dimension(x) == Dimensions(R, length=1, mass=5 // 2)
        if R == DEFAULT_DIM_TYPE
            @test x == Q(T(0.2), length=1, mass=2.5)
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

        y = -x

        @test ustrip(y) == -ustrip(x)
        @test dimension(y) == dimension(x)

        y = x / x

        @test iszero(x.dimensions) == false
        @test iszero(y.dimensions) == true

        y = Q(T(2 // 10), D, length=1, mass=5 // 2)

        @test y ≈ x

        y = Q(T(2 // 10), D, length=1, mass=6 // 2)

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

        dimensionless = Q(one(T), D)
        y = T(2) + dimensionless
        @test ustrip(y) == T(3)
        @test dimension(y) == Dimensions(R)
        @test typeof(y) == Q{T,D}

        y = T(2) - dimensionless
        @test ustrip(y) == T(1)
        @test dimension(y) == Dimensions(R)
        @test typeof(y) == Q{T,D}

        y = dimensionless + T(2)
        @test ustrip(y) == T(3)
        y = dimensionless - T(2)
        @test ustrip(y) == T(-1)

        @test_throws DimensionError Q(one(T), D,  length=1) + 1.0
        @test_throws DimensionError Q(one(T), D, length=1) - 1.0
        @test_throws DimensionError 1.0 + Q(one(T), D, length=1)
        @test_throws DimensionError 1.0 - Q(one(T), D, length=1)
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
    @test isreal(x) == true
    @test isreal(x * (1 + 2im)) == false
    @test signbit(x) == true
    @test signbit(-x) == false
    @test isempty(x) == false
    @test isempty(GenericQuantity([0.0, 1.0])) == false
    @test isempty(GenericQuantity(Float64[])) == true
    @test iseven(Quantity(2, length=1)) == true
    @test iseven(Quantity(3, length=1)) == false
    @test isodd(Quantity(2, length=1)) == false
    @test isodd(Quantity(3, length=1)) == true
    @test isinteger(Quantity(2, length=1)) == true
    @test isinteger(Quantity(2.1, length=1)) == false
    @test ispow2(Quantity(2, length=1)) == true
    @test ispow2(Quantity(3, length=1)) == false

    @test nextfloat(x) == Quantity(nextfloat(-1.2), length=2 // 5)
    @test prevfloat(x) == Quantity(prevfloat(-1.2), length=2 // 5)

    y = Quantity(-1, mass=1)
    @test unsigned(y) == Quantity(unsigned(-1), mass=1)

    z = Quantity(-0.3)
    @test float(z) == z
    @test convert(Float32, z) === convert(Float32, -0.3)

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
    @test angle(x) == angle(ustrip(x))
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

        x = GenericQuantity(ones(T, 32))
        @test ustrip(x + ones(T, 32))[32] == 2
        @test typeof(x + ones(T, 32)) <: GenericQuantity{Vector{T}}
        @test typeof(x - ones(T, 32)) <: GenericQuantity{Vector{T}}
        @test typeof(ones(T, 32) * GenericQuantity(T(1), D, length=1)) <: GenericQuantity{Vector{T}}
        @test typeof(ones(T, 32) / GenericQuantity(T(1), D, length=1)) <: GenericQuantity{Vector{T}}
        @test ones(T, 32) / GenericQuantity(T(1), length=1) == GenericQuantity(ones(T, 32), length=-1)
    end

    x = randn(32) .* u"km/s"
    @test ustrip.(x) == [ustrip(xi) for xi in x]
    @test dimension.(x) == [dimension(u"km/s") for xi in x]
    @test_throws DimensionError dimension([u"km/s", u"km"])

    @test norm(x, 2) ≈ norm(ustrip.(x), 2) * u"m/s"
    @test norm(GenericQuantity(ustrip.(x), length=1, time=-1), 2) ≈ norm(ustrip.(x), 2) * u"m/s"

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
    @test typeof(one(GenericQuantity(ones(32, 32)))) == GenericQuantity{Matrix{Float64},DEFAULT_DIM_TYPE}
    @test dimension(one(GenericQuantity(ones(32, 32), length=1))) == Dimensions()

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
    @test convert(Bool, FixedRational{Int8,6}(1//1)) === true
    @test convert(Bool, FixedRational{Int8,6}(0//1)) === false

    @test_throws InexactError convert(Int32, FixedRational{Int8,6}(2//3))
    @test_throws InexactError convert(Bool, FixedRational{Int8,6}(2//1))

    VERSION >= v"1.8" && @test_throws "Refusing to" promote(FixedRational{Int,10}(2), FixedRational{Int,4}(2))

    f64 = FixedRational{Int,10}(2)
    f8 = FixedRational{Int8,10}(2)
    @test promote(f64, f8) == (2, 2)
    @test typeof(promote(f64, f8)) == typeof((f64, f64))
    @test typeof(promote(FixedRational{Int8,10}(2), FixedRational{Int8,10}(2))) == typeof((f8, f8))
    @test promote_type(Float64, typeof(f64)) == Float64

    # Required to hit integer branch (otherwise will go to `literal_pow`)
    f(i::Int) = Dimensions(length=1, mass=-1)^i
    @test f(2) == Dimensions(length=2, mass=-2)

    # Null conversion
    @test typeof(FixedRational{Int,10}(FixedRational{Int,10}(2))) == FixedRational{Int,10}

    # Conversion to Rational without specifying type
    @test convert(Rational, FixedRational{UInt8,6}(2)) === Rational{UInt8}(2)

    @test record_show(FixedRational{Int,10}(2)) == "2"
    @test record_show(FixedRational{Int,10}(11//10)) == "11//10"

    # Promotion rules
    @test promote_type(FixedRational{Int64,10},FixedRational{BigInt,10}) == FixedRational{BigInt,10}
    @test promote_type(Rational{Int8}, FixedRational{Int,12345}) == Rational{Int}
    @test promote_type(Int8, FixedRational{Int,12345}) == FixedRational{Int,12345}
end

@testset "Quantity promotion" begin
    q1 = Quantity(1.0, length=1)
    q2 = Quantity(2, mass=1)
    @test typeof(promote(q1, q2)) == typeof((q1, q1))

    q = 0.5u"km/s"
    x = [0.5, q]
    @test x isa Vector{typeof(q)}
    @test x[1] == convert(typeof(q), 0.5)

    q = GenericQuantity(0.5u"km/s")
    x = [0.5, q]
    @test x isa Vector{typeof(q)}

    # Promotion with custom numeric type:
    @eval struct MyNumber <: Real
        x::Float64
    end
    a = 0.5u"km/s"
    b = MyNumber(0.5)
    ar = [a, b]
    @test ar isa Vector{Number}
    @test a === ar[1]
    @test b === ar[2]
    @test promote_type(MyNumber, typeof(a)) == Number

    # Explicit conversion so coverage can see it:
    D = DEFAULT_DIM_TYPE
    @test promote_type(Quantity{Float32,D}, Float64) == Quantity{Float64,D}
    @test promote_type(Quantity{Float32,D}, Quantity{Float64,D}) == Quantity{Float64,D}
    @test promote_type(Quantity{Float32,D}, GenericQuantity{Float64,D}) == GenericQuantity{Float64,D}
    @test promote_type(GenericQuantity{Float32,D}, GenericQuantity{Float64,D}) == GenericQuantity{Float64,D}
    @test promote_type(SymbolicDimensions{Rational{Int}}, SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}) == SymbolicDimensions{Rational{Int}}
    @test promote_type(Dimensions{Rational{Int}}, SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}) == Dimensions{Rational{Int}}
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

    # Can use the default constructorof, with_type_parameters, and dimension_names:
    @test DynamicQuantities.constructorof(MyDimensions{Float64}) == MyDimensions
    @test DynamicQuantities.constructorof(MyQuantity{Float64}) == MyQuantity
    @test DynamicQuantities.with_type_parameters(MyDimensions{Float64}, Rational{Int}) == MyDimensions{Rational{Int}}
    @test DynamicQuantities.with_type_parameters(MyQuantity{Float64,DEFAULT_DIM_TYPE}, Float32, MyDimensions{Float64}) == MyQuantity{Float32,MyDimensions{Float64}}
    @test DynamicQuantities.dimension_names(MyDimensions{Float64}) == (:length, :mass, :time)

    # But, we always need to use a quantity when mixing with mathematical operations:
    @test_throws ErrorException MyQuantity(0.1) + 0.1 * MyDimensions()
end

@testset "Symbolic dimensions" begin
    # TODO: Remove constructors for sym3 and sym4s?
    sym1 = @inferred(SymbolicDimensions(; m=3, s=-1))
    sym2 = @inferred(SymbolicDimensions{Rational{Int}}(; m=3, s=-1))
    sym3 = @inferred(SymbolicDimensions(Rational{Int}; m=3, s=-1))
    sym4 = @inferred(SymbolicDimensions{Int}(Rational{Int}; m=3, s=-1))
    for (sym, T) in (
        (sym1, DynamicQuantities.DEFAULT_DIM_BASE_TYPE), (sym2, Rational{Int}), (sym3, Rational{Int}), (sym4, Rational{Int}),
    )  
        @test sym isa SymbolicDimensions{T}

        # Properties
        @test sym.m == 3
        @test sym.s == -1
        @test propertynames(sym) == DynamicQuantities.ALL_SYMBOLS
        @test issubset((:m, :s), propertynames(sym))
        @test all(propertynames(sym)) do x
            val = getproperty(sym, x)
            return x === :m ? val == 3 : (x === :s ? val == -1 : iszero(val))
        end

        # Internal constructor
        @test DynamicQuantities.constructorof(typeof(sym)) === SymbolicDimensions

        # Equality comparisons
        @test sym == sym
        @test sym == copy(sym)
        @test sym !== copy(sym)
        @test sym == SymbolicDimensions{Int}(; m=3, s=-1)
        @test SymbolicDimensions{Int}(; m=3, s=-1) == sym
        @test sym == SymbolicDimensions(; m=3, g=0, s=-1)
        @test SymbolicDimensions(; m=3, g=0, s=-1) == sym
        @test sym == SymbolicDimensions(; m=3, s=-1, K=0)
        @test SymbolicDimensions(; m=3, s=-1, K=0) == sym
        @test sym != SymbolicDimensions(; m=2, s=-1)
        @test SymbolicDimensions(; m=2, s=-1) != sym
        @test sym != SymbolicDimensions(; m=3, g=1, s=-1)
        @test SymbolicDimensions(; m=3, g=1, s=-1) != sym
        @test sym != SymbolicDimensions(; m=3, s=-1, K=1)
        @test SymbolicDimensions(; m=3, s=-1, K=1) != sym

        @test !iszero(sym)
    end

    q = 1.5us"km/s"
    @test q == 1.5 * us"km" / us"s"
    @test typeof(q) <: Quantity{Float64,<:SymbolicDimensions}
    @test string(dimension(q)) == "s⁻¹ km"
    @test uexpand(q) == 1.5u"km/s"
    @test string(dimension(us"Constants.au^1.5")) == "au³ᐟ²"
    @test string(dimension(uexpand(us"Constants.au^1.5"))) == "m³ᐟ²"
    @test uexpand(2.3us"Constants.au^1.5") ≈ 2.3u"Constants.au^1.5"
    @test iszero(dimension(us"1.0")) == true
    @test uexpand(inv(us"Constants.au")) ≈ 1/u"Constants.au"
    @test dimension(inv(us"s") * us"km") == dimension(us"km/s")
    @test dimension(inv(us"s") * us"m") != dimension(us"km/s")
    @test dimension(uexpand(inv(us"s") * us"m")) == dimension(uexpand(us"km/s"))

    f2(i::Int) = us"s"^i
    @inferred f2(5)
    @test uexpand(f2(5)) == u"s"^5

    @test_throws ErrorException sym_uparse("'c'")

    # For constants which have a namespace collision, the numerical expansion is used:
    @test dimension(us"Constants.au")[:au] == 1
    @test dimension(us"Constants.h")[:h] == 0
    @test dimension(us"h")[:h] == 1

    @test us"Constants.h" != us"h"
    @test uexpand(us"Constants.h") == u"Constants.h"

    # Actually expands to:
    @test dimension(us"Constants.h")[:m] == 2
    @test dimension(us"Constants.h")[:s] == -1
    @test dimension(us"Constants.h")[:kg] == 1

    # So the numerical value is different from other constants:
    @test ustrip(us"Constants.h") == ustrip(u"Constants.h")
    @test ustrip(us"Constants.au") != ustrip(u"Constants.au")

    # Test conversion
    @test typeof(SymbolicDimensions{Rational{Int}}(dimension(us"km/s"))) == SymbolicDimensions{Rational{Int}}
    @test convert(Quantity{Float64,SymbolicDimensions}, u"kg") == 1.0us"kg"
    @test convert(Quantity{Float64,SymbolicDimensions}, u"cm") == 1e-2us"m"
    @test convert(Quantity{Float64,Dimensions}, 3.5us"kg/s") == 3.5u"kg/s"
    @test convert(Quantity{Float64,Dimensions}, 3.5us"Constants.pc") == 3.5u"Constants.pc"

    # Helpful error if symbol not found:
    sym5 = dimension(us"km/s")
    VERSION >= v"1.8" &&
        @test_throws "rad is not available as a symbol" sym5.rad

    # Test deprecated method
    q = 1.5us"km/s"
    @test expand_units(q) == uexpand(q)

    # Test promotions:
    x = Quantity{Float32,SymbolicDimensions{Rational{Int}}}(0.2us"km/s")
    y = 0.5us"km/s"
    qa = [x, y]
    @test qa isa Vector{Quantity{Float64,SymbolicDimensions{Rational{Int}}}}
    DynamicQuantities.with_type_parameters(SymbolicDimensions{Float64}, Rational{Int}) == SymbolicDimensions{Rational{Int}}
end

@testset "uconvert" begin
    @test uconvert(us"nm", 5e-9u"m") ≈ (5e-9u"m" |> uconvert(us"nm")) ≈ 5us"nm"
    @test_throws DimensionError uconvert(us"nm * J", 5e-9u"m")

    # Types:
    @test typeof(uconvert(us"nm", 5e-9u"m")) <: Quantity{Float64,<:SymbolicDimensions}
    @test typeof(uconvert(us"nm", GenericQuantity(5e-9u"m"))) <: GenericQuantity{Float64,<:SymbolicDimensions}
    @test uconvert(GenericQuantity(us"nm"), GenericQuantity(5e-9u"m")) ≈ 5us"nm"
    @test uconvert(GenericQuantity(us"nm"), GenericQuantity(5e-9u"m")) ≈ GenericQuantity(5us"nm")

    # We only want to convert the dimensions, and ignore the quantity type:
    @test typeof(uconvert(GenericQuantity(us"nm"), 5e-9u"m")) <: Quantity{Float64,<:SymbolicDimensions}

    q = 1.5u"Constants.M_sun"
    qs = uconvert(us"Constants.M_sun", 5.0 * q)
    @test qs ≈ 7.5us"Constants.M_sun"
    @test dimension(qs)[:kg] == 0
    @test dimension(qs)[:g] == 0
    @test dimension(qs)[:M_sun] == 1
    @test uexpand(qs) ≈ 5.0 * q

    # Refuses to convert to non-unit quantities:
    @test_throws AssertionError uconvert(1.2us"m", 1.0u"m")
    VERSION >= v"1.8" &&
        @test_throws "You passed a quantity" uconvert(1.2us"m", 1.0u"m")

    for Q in (Quantity, GenericQuantity)
        # Different types require converting both arguments:
        q = convert(Q{Float16}, 1.5u"g")
        qs = uconvert(convert(Q{Float16}, us"g"), 5 * q)
        @test typeof(qs) <: Q{Float16,<:SymbolicDimensions{<:Any}}
        @test qs ≈ 7.5us"g"

        # Arrays
        x = [1.0, 2.0, 3.0] .* Q(u"kg")
        xs = x .|> uconvert(us"g")
        @test typeof(xs) <: Vector{<:Q{Float64,<:SymbolicDimensions{<:Any}}}
        @test xs[2] ≈ Q(2000us"g")

        x_qa = QuantityArray(x)
        xs_qa = x_qa .|> uconvert(us"g")
        @test typeof(xs_qa) <: QuantityArray{Float64,1,<:SymbolicDimensions{<:Any}}
        @test xs_qa[2] ≈ Q(2000us"g")

        # Without vectorized call:
        xs_qa2 = x_qa |> uconvert(us"g")
        @test typeof(xs_qa2) <: QuantityArray{Float64,1,<:SymbolicDimensions{<:Any}}
        @test xs_qa2[2] ≈ Q(2000us"g")
    end
end

@testset "Test missing" begin
    x = 1.0u"m"
    @test round(Union{Int32,Missing}, FixedRational{Int32,100}(1)) isa Int32
    @test round(Union{Int32,Missing}, FixedRational{Int32,100}(1)) === Int32(1)
    y = missing
    @test isless(x, y) === missing
    @test isless(y, x) === missing
    @test (x == y) === missing
    @test (y == x) === missing
    @test isapprox(x, y) === missing
    @test isapprox(y, x) === missing

    x = 1.0u"m"
    y = missing
    @test x * y === missing
    @test x + y === missing
end


@testset "Test ambiguities" begin
    R = DEFAULT_DIM_BASE_TYPE
    x = convert(R, 10)
    y = convert(R, 5)
    @test promote(x, y) == (x, y)
    @test_throws ErrorException promote(x, convert(FixedRational{Int32,100}, 10))
    @test promote_type(typeof(u"km/s"), typeof(convert(Quantity{Float32}, u"km/s"))) <: Quantity{Float64}

    x = 1.0u"m"
    s = "test"
    y = WeakRef(s)
    @test_throws ErrorException x == y
    @test_throws ErrorException y == x

    qarr1 = QuantityArray(randn(3), u"km/s")
    qarr2 = qarr1
    @test convert(typeof(qarr2), qarr2) === qarr1

    x = 1.0u"m"
    y = x ^ (3//2)
    @test y == Quantity(1.0, length=3//2)
    @test typeof(y) == Quantity{Float64,DEFAULT_DIM_TYPE}
end

for Q in (Quantity, GenericQuantity)
    @testset "Arrays" begin
        @testset "Basics" begin
            x = QuantityArray(randn(32), Q(u"km/s"))
            @test ustrip(sum(x)) ≈ sum(ustrip(x))

            # Setting index with different quantity:
            x[5] = Q(5, length=1, time=-1)
            @test x[5] == Q(5, length=1, time=-1)

            y = randn(32)
            @test ustrip(QuantityArray(y, Q(u"m"))) == y

            f_square(v) = v^2 * 1.5 - v^2
            @test sum(f_square.(QuantityArray(y, Q(u"m")))) == sum(f_square.(y) .* Q(u"m^2"))

            y_q = QuantityArray(y, Q(u"m * cd / s"))
            @test typeof(f_square.(y_q)) == typeof(y_q)

            for get_u in (ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount)
                @test get_u(f_square.(y_q)) == get_u(y_q) * 2
                @test get_u(f_square(first(y_q))) == get_u(y_q) * 2
            end

            # Test default constructors:
            @test QuantityArray(ones(3), u"m/s") == QuantityArray(ones(3), length=1, time=-1)
            @test typeof(QuantityArray(ones(3), u"m/s")) <: QuantityArray{Float64,1,<:Dimensions,<:Quantity,<:Array}

            # We can create quantity arrays with generic quantity
            @test typeof(QuantityArray([[1.0], [2.0, 3.0]], dimension(u"m/s"))) <: QuantityArray{<:Any,1,<:Dimensions,<:GenericQuantity,<:Array}
            @test typeof(QuantityArray([[1.0], [2.0, 3.0]], GenericQuantity(u"m/s"))) <: QuantityArray{<:Any,1,<:Dimensions,<:GenericQuantity,<:Array}
            @test QuantityArray([[1.0], [2.0, 3.0]], GenericQuantity(u"km/s"))[1][1] == 1000u"m/s"

            fv_square(v) = f_square.(v)
            @inferred fv_square(y_q)

            # Same array type:
            s_x = QuantityArray(SArray{Tuple{32}}(ustrip(x)), dimension(x), Q)
            @test typeof(s_x) <: QuantityArray{Float64,1,<:Dimensions,<:Q{Float64,<:Dimensions},<:SArray}
            output_s_x = (xi -> xi^2).(s_x)
            @test array_type(output_s_x) <: MArray
            @test dim_type(output_s_x) <: Dimensions{<:FixedRational}
            @test value_type(output_s_x) == Float64
            @test dimension(output_s_x) == dimension(x)^2
            fv_square2(x) = (xi -> xi^2).(x)
            @inferred fv_square2(s_x)
        end

        @testset "Copying" begin
            x = QuantityArray(randn(3), Q(u"km/s"))
            xc = copy(x)
            @test x == xc
            xc[2] *= 0.5
            @test x != xc
        end

        @testset "Utilities" begin
            @test fill(Q(u"m/s"), 10) == QuantityArray(fill(1.0, 10) .* Q(u"m/s"))
            @test ndims(fill(Q(u"m/s"), ())) == 0
            @test fill(Q(u"m/s"), ())[begin] == Q(u"m/s")
        end

        @testset "similar" begin
            qa = QuantityArray(rand(3, 4), Q(u"m"))

            new_qa = similar(qa)
            @test size(new_qa) == size(qa)
            @test eltype(new_qa) == eltype(qa)
            @test dim_type(new_qa) == dim_type(qa)
            @test quantity_type(new_qa) == eltype(new_qa)
            @test dimension(new_qa) == dimension(qa)
            @test isa(ustrip(new_qa), Array{Float64,2})
            @test !isequal(ustrip(qa), ustrip(new_qa))

            new_qa = similar(qa, Float32)
            @test eltype(new_qa) <: Q{Float32}
            @test dim_type(new_qa) == dim_type(qa)
            @test dimension(new_qa) == dimension(qa)
            @test isa(ustrip(new_qa), Array{Float32,2})

            new_qa = similar(qa, axes(ones(6, 8)))
            @test size(new_qa) == (6, 8)
            @test eltype(new_qa) <: Q{Float64}
            @test dim_type(new_qa) == dim_type(qa)
            @test dimension(new_qa) == dimension(qa)
            @test isa(ustrip(new_qa), Array{Float64,2})

            new_qa = similar(qa, Float32, axes(ones(6, 8)))
            @test size(new_qa) == (6, 8)
            @test eltype(new_qa) <: Q{Float32}

            new_qa = similar(qa, Float32, (6,))
            @test size(new_qa) == (6,)
            @test eltype(new_qa) <: Q{Float32}

            new_qa = similar(qa, (6,))
            @test size(new_qa) == (6,)
            @test eltype(new_qa) <: Q{Float64}

            new_qa = similar(qa, Float32, (6, UInt(3)))
            @test size(new_qa) == (6, 3)
            @test eltype(new_qa) <: Q{Float32}

            new_qa = similar(qa, (6, UInt(3)))
            @test size(new_qa) == (6, 3)
            @test eltype(new_qa) <: Q{Float64}
        end

        @testset "Promotion" begin
            qarr1 = QuantityArray(randn(32), convert(Dimensions{Rational{Int32}}, dimension(u"km/s")), Q)
            qarr2 = QuantityArray(randn(Float16, 32), convert(Dimensions{Rational{Int64}}, dimension(u"km/s")), Q)

            expected_T = Float64
            expected_D = Dimensions{Rational{Int64}}
            expected_type = QuantityArray{expected_T,1,expected_D,Q{Float64,expected_D},Array{expected_T,1}}

            @test promote_type(typeof(qarr1), typeof(qarr2)) == expected_type
            @test typeof(promote(qarr1, qarr2)) == Tuple{expected_type, expected_type}
        end

        @testset "Array concatenation" begin
            qarr1 = QuantityArray(randn(3) .* Q(u"km/s"))
            qarr2 = QuantityArray(randn(3) .* Q(u"km/s"))

            @test ustrip.(hcat(qarr1, qarr2)) == hcat(ustrip(qarr1), ustrip(qarr2))
            @test ustrip.(vcat(qarr1, qarr2)) == vcat(ustrip(qarr1), ustrip(qarr2))
            @test ustrip.(cat(qarr1, qarr2, dims=2)) == cat(ustrip(qarr1), ustrip(qarr2), dims=Val(2))
            @test dimension(hcat(qarr1, qarr2)) == dimension(u"km/s")

            # type stability:
            @inferred hcat(qarr1, qarr2)
            @inferred vcat(qarr1, qarr2)
            @inferred cat(qarr1, qarr2, dims=Val(2))

            # same array type:
            s_qarr1 = QuantityArray(SArray{Tuple{3}}(ustrip(qarr1)), dimension(qarr1), Q)
            s_qarr2 = QuantityArray(SArray{Tuple{3}}(ustrip(qarr2)), dimension(qarr2), Q)
            @test array_type(hcat(s_qarr1, s_qarr2)) <: SArray

            # Test concatenating different arrays:

        end

        @testset "Generic literal_pow" begin
            y = randn(32)
            y_q = QuantityArray(y, Q(u"m"))

            f4(v) = v^4 * 0.3
            @test sum(f4.(QuantityArray(y, Q(u"m")))) == sum(f4.(y) .* Q(u"m^4"))

            f4v(v) = f4.(v)
            @inferred f4v(y_q)
        end

        @testset "Broadcast with single number" begin
            ar1 = QuantityArray(randn(3), Q(u"km/s"))
            @test ustrip(ar1 .* Q(u"m/s")) == ustrip(ar1)
            @test dimension(ar1 .* Q(u"m/s")) == dimension(u"m^2/s^2")
        end

        @testset "Multiple arrays" begin
            ar1 = QuantityArray(randn(3), Q(u"km/s"))
            ar2 = QuantityArray(randn(3, 1), Q(u"km/s"))
            ar3 = randn(3)
            f(x, y, z) = x + y * z
            g(x, y, z) = f.(x, y, z)
            @inferred g(ar1, ar2[:, 1], ar3)
            @test g(ar1, ar2[:, 1], ar3) == [f(ar1[i], ar2[i, 1], ar3[i]) for i in eachindex(ar1)]

            tuple_array = Q.((1u"km/s", 2u"m/s", 3u"cm/s"))
            @inferred g(ar1, tuple_array, ar3)
            @test g(ar1, tuple_array, ar3) == [f(ar1[i], tuple_array[i], ar3[i]) for i in eachindex(ar1)]

            array_of_quantities = Q.([1u"km/s", 2u"m/s", 3u"cm/s"])
            @inferred g(ar1, array_of_quantities, ar3)
            @test g(ar1, tuple_array, ar3) == g(ar1, array_of_quantities, ar3)

            @test typeof(Base.broadcasted(f, ar1, array_of_quantities, 1.0).args[end]) == Float64
            q = Q(u"1")
            @test typeof(Base.broadcasted(f, ar1, array_of_quantities, q).args[end]) == typeof(q)

            # TODO: Type inference here needs to be fixed
            @test_skip @inferred g(ar1, array_of_quantities, Q(u"1"))
            @test g(ar1, array_of_quantities, Q(u"1")) == [f(ar1[i], array_of_quantities[i], 1) for i in eachindex(ar1)]
        end

        @testset "Broadcast nd-arrays" begin
            x = QuantityArray(randn(3, 3), Q(u"A"))
            y = QuantityArray(randn(3, 3), Q(u"cd"))
            @test ustrip(x .* y) == ustrip(x) .* ustrip(y)
        end

        Q == Quantity && @testset "Broadcast different arrays" begin
            f(x, y, z, w) = x * y + z * w
            g(x, y, z, w) = f.(x, y, z, w)

            x = randn(32)
            y = QuantityArray(randn(32), u"km/s")
            z = rand(1:10, 32)
            w = Quantity{Float32}(u"m/s")
            @test typeof(g(x, y, z, w)) <: QuantityArray{Float64}

            y32 = QuantityArray(ustrip(y), dimension(y))
            @test typeof(y .* y32) <: QuantityArray{Float64}

            a = [randn() * u"km/s" for i=1:32]
            @test typeof(y .* a) <: QuantityArray
            @test typeof(a .* y) <: QuantityArray

            b = GenericQuantity(randn(Float32, 32), length=1, time=-1)
            @test typeof(b) <: GenericQuantity
            @test typeof(b .* b) <: Vector{<:GenericQuantity}
            @test typeof(a .* b) <: Vector{<:GenericQuantity}
            @test typeof(b .* a) <: Vector{<:GenericQuantity}
            @test typeof(y .* b) <: QuantityArray{Float64}
            @test typeof(b .* y) <: QuantityArray{Float64}
        end

        Q == Quantity && @testset "Broadcast scalars" begin
            for (x, qx) in ((0.5, 0.5u"s"), ([0.5, 0.2], GenericQuantity([0.5, 0.2], time=1)))
                @test size(qx) == size(x)
                @test length(qx) == length(x)
                @test axes(qx) == axes(x)
                @test iterate(qx)[1] == (iterate(x)[1] * u"s")
                @test ndims(qx) == ndims(x)
                @test Base.broadcastable(qx) == qx
                ustrip(qx) isa Real && @test qx[1] == qx
                @test keys(qx) == keys(x)
            end
        end

        @testset "Symbolic units" begin
            z_ar = randn(32)
            z = QuantityArray(z_ar, Q(us"Constants.h * km/s"))
            z_expanded = QuantityArray(z_ar .* Q(u"Constants.h * km/s"))
            @test typeof(uexpand(z)) == typeof(z_expanded)
            @test all(uexpand(z) .≈ z_expanded)
            io = IOBuffer()
            Base.showarg(io, z, true)
            msg = String(take!(io))
            Q == Quantity && @test occursin(r"QuantityArray\(::Vector{Float64}, ::(DynamicQuantities\.)?Quantity{Float64, (DynamicQuantities\.)?SymbolicDimensions{(DynamicQuantities\.)?FixedRational{Int32, 25200}}}\)", msg)

            io = IOBuffer()
            Base.show(io, MIME"text/plain"(), typeof(z))
            msg2 = String(take!(io))
            @test msg2 == msg
        end

        Q == Quantity && @testset "Extra test coverage" begin
            @test_throws ErrorException DynamicQuantities.materialize_first(())
            VERSION >= v"1.8" &&
                @test_throws "Unexpected broadcast" DynamicQuantities.materialize_first(())

            # Not sure how to test this otherwise, but method is supposed to be
            # required for the broadcasting interface
            x = [1u"km/s"]
            ref = Base.RefValue(x)
            @test DynamicQuantities.materialize_first(ref) === x[1]
        end
    end
end

@testset "GenericQuantity" begin
    @testset "GenericQuantity construction" begin
        x = GenericQuantity(1.5)
        @test x isa GenericQuantity
        @test ustrip(x) == 1.5
        @test dimension(x) == Dimensions()

        x = GenericQuantity(big(1.5)) 
        @test typeof(x) <: GenericQuantity{BigFloat}

        x = GenericQuantity([1.5, 2.0], Dimensions{Rational{Int8}}; length=1)
        @test x isa GenericQuantity{Vector{Float64},Dimensions{Rational{Int8}}}
        @test supertype(typeof(x)) <: AbstractGenericQuantity{Vector{Float64},Dimensions{Rational{Int8}}}
        @test ustrip(x) == [1.5, 2.0]
        @test dimension(x) == Dimensions(length=1)

        x = GenericQuantity(randn(3,3))
        @test x isa GenericQuantity{Matrix{Float64}}
        @test size(x) == (3,3)

        x = GenericQuantity(1.5, length=1)
        y = GenericQuantity(2.0, time=1)
        @test_throws DimensionError x + y

        x = 0.5us"km/s"
        y = GenericQuantity(1.0)

    end

    @testset "GenericQuantity and Quantity promotion" begin
        x = GenericQuantity(1.5f0)
        y = Quantity(1.5, length=1)

        # *Always* promotes to GenericQuantity:
        @test typeof(x * y) <: GenericQuantity{Float64}

        x = GenericQuantity(rand(3))
        y = GenericQuantity(rand(3), length=1)

        @test typeof(x .* y) <: Vector{<:GenericQuantity{Float64}}

        x = 0.5us"km/s"
        @test GenericQuantity(x) isa GenericQuantity{Float64}

        x = GenericQuantity("abcd"; length=1)
        @test x isa GenericQuantity{String}
        y = "c"
        @test x * y == GenericQuantity("abcdc"; length=1)

        x = GenericQuantity([1.0, 2.0]; length=1)
        y = Quantity(3; mass=-1)
        @test x * y isa GenericQuantity{Vector{Float64}}
        @test x * y == GenericQuantity([3.0, 6.0]; length=1, mass=-1)

        x = [GenericQuantity(1.0; length=1), Quantity(2.0; length=1)]
        @test x isa Vector{<:GenericQuantity}
        ax = QuantityArray(x)
        @test ax isa QuantityArray{Float64,1,<:Dimensions,<:GenericQuantity{Float64}}

        # Test both symbolic/non-symbolic and Quantity/GenericQuantity:
        x = [0.5us"km/s", GenericQuantity(1.0f0; length=1)]
        @test x isa Vector{<:GenericQuantity{Float64,<:Dimensions}}
        @test ustrip(x[1]) == 500.0
        @test ustrip(x[2]) == 1.0

        x = [GenericQuantity([1.0, 2.0]), GenericQuantity([3f0, 4f0], Dimensions{Rational{Int}}, length=1)]
        @test x isa Vector{GenericQuantity{Vector{Float64},Dimensions{Rational{Int}}}}

        # Explicitly trigger Number conversion:
        x = Quantity(1u"nm")
        @test convert(Number, x) === x
        @test x isa Number
    end

    @testset "GenericQuantity broadcasting" begin
        x = QuantityArray([GenericQuantity(1.0f0i; length=1) for i=1:30])
        y = randn(30)
        z = QuantityArray([Quantity(1.0f0i; length=1) for i=1:30])

        @test x isa AbstractArray{<:GenericQuantity}
        @test x isa QuantityArray{Float32,1,<:Dimensions,<:GenericQuantity{Float32}}
        @test x .* y isa QuantityArray{Float64,1,<:Dimensions,<:GenericQuantity{Float64}}

        # TODO: Currently this converts to a `Vector` of `GenericQuantity`
        @test_skip x .* z isa QuantityArray{Float32,1,<:Dimensions,<:GenericQuantity{Float32}}
    end

    @testset "Array conversion" begin
        x = SArray{Tuple{3}}(randn(3))
        y = SArray{Tuple{3}}(randn(Float32, 3))
        qx = QuantityArray(x, Dimensions(Rational{Int}, length=1))
        qy = QuantityArray(y; length=1)

        @test typeof(convert(typeof(qx), qy)) == typeof(qx)
        @test convert(typeof(qx), qy)[1] isa Quantity{Float64}
        @test convert(typeof(qx), qy)[1] == convert(Quantity{Float64}, qy[1])
    end
end

function is_input_valid(f, x)
    try
        f(x)
    catch e
        e isa DomainError && return false
        rethrow(e)
    end
    return true
end

@testset "Assorted dimensionless functions" begin
    functions = (
        :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos,
        :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
        :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
        :sinpi, :cospi, :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind,
        :log, :log2, :log10, :log1p, :exp, :exp2, :exp10, :expm1, :frexp, :exponent,
        :atan, :atand
    )
    for Q in (Quantity, GenericQuantity), D in (Dimensions, SymbolicDimensions), f in functions
        # Only test on valid domain
        valid_inputs = filter(
            x -> is_input_valid(eval(f), x),
            5rand(100) .- 2.5
        )
        for x in valid_inputs[1:3]
            qx_dimensionless = Quantity(x, D)
            qx_dimensions = Quantity(x, convert(D, dimension(u"m/s")))
            @eval @test $f($qx_dimensionless) == $f($x)
            @eval @test_throws DimensionError $f($qx_dimensions)
            if f in (:atan, :atand)
                for y in valid_inputs[end-3:end]
                    qy_dimensionless = Quantity(y, D)
                    qy_dimensions = Quantity(y, convert(D, dimension(u"m/s")))
                    @eval @test $f($y, $qx_dimensionless) == $f($y, $x)
                    @eval @test $f($qy_dimensionless, $x) == $f($y, $x)
                    @eval @test $f($qy_dimensionless, $qx_dimensionless) == $f($y, $x)
                    @eval @test $f($qy_dimensions, $qx_dimensions) == $f($y, $x)
                    @eval @test_throws DimensionError $f($qy_dimensions, $x)
                    @eval @test_throws DimensionError $f($y, $qx_dimensions)
                end
            end
        end
    end
    s = record_show(DimensionError(u"km/s"), showerror)
    @test occursin("not dimensionless", s)
end

@testset "Assorted dimensionful functions" begin
    functions = (
        :float, :abs, :real, :imag, :conj, :adjoint, :unsigned,
        :nextfloat, :prevfloat, :identity, :transpose,
        :copysign, :flipsign, :mod, :modf,
        :floor, :trunc, :ceil, :significand,
        :ldexp, :round,
    )
    for Q in (Quantity, GenericQuantity), D in (Dimensions, SymbolicDimensions), f in functions
        T = f in (:abs, :real, :imag, :conj) ? ComplexF64 : Float64
        if f == :modf  # Functions that return multiple outputs
            for x in 5rand(T, 3) .- 2.5
                dim = convert(D, dimension(u"m/s"))
                qx_dimensions = Q(x, dim)
                num_outputs = 2
                for i=1:num_outputs
                    @eval @test $f($qx_dimensions)[$i] == $Q($f($x)[$i], $dim)
                end
            end
        elseif f in (:copysign, :flipsign, :rem, :mod)  # Functions that need multiple inputs
            for x in 5rand(T, 3) .- 2.5
                for y in 5rand(T, 3) .- 2.5
                    dim = convert(D, dimension(u"m/s"))
                    qx_dimensions = Q(x, dim)
                    qy_dimensions = Q(y, dim)
                    @eval @test $f($qx_dimensions, $qy_dimensions) == $Q($f($x, $y), $dim)
                    if f in (:copysign, :flipsign, :mod)
                        # Also do test without dimensions
                        @eval @test $f($x, $qy_dimensions) == $f($x, $y)
                        @eval @test $f($qx_dimensions, $y) == $Q($f($x, $y), $dim)
                    end
                end
            end
        elseif f == :unsigned
            for x in 5rand(-10:10, 3)
                dim = convert(D, dimension(u"m/s"))
                qx_dimensions = Q(x, dim)
                @eval @test $f($qx_dimensions) == $Q($f($x), $dim)
            end
        elseif f in (:round, :floor, :trunc, :ceil)
            for x in 5rand(T, 3) .- 2.5
                dim = convert(D, dimension(u"m/s"))
                qx_dimensions = Q(x, dim)
                @eval @test $f($qx_dimensions) == $Q($f($x), $dim)
                @eval @test $f(Int32, $qx_dimensions) == $Q($f(Int32, $x), $dim)
            end
        elseif f == :ldexp
            for x in 5rand(T, 3) .- 2.5
                dim = convert(D, dimension(u"m/s"))
                qx_dimensions = Q(x, dim)
                for i=1:3
                    @eval @test $f($qx_dimensions, $i) == $Q($f($x, $i), $dim)
                end
            end
        else
            # Only test on valid domain
            valid_inputs = filter(
                x -> is_input_valid(eval(f), x),
                5rand(T, 100) .- 2.5
            )
            for x in valid_inputs[1:3]
                dim = convert(D, dimension(u"m/s"))
                qx_dimensions = Q(x, dim)
                @eval @test $f($qx_dimensions) == $Q($f($x), $dim)
            end
        end
    end
end

@testset "Test div" begin
    for Q in (Quantity, GenericQuantity)
        x = Q{Int}(10, length=1)
        y = Q{Int}(3, mass=-1)
        @test div(x, y) == Q{Int}(3, length=1, mass=1)
        @test div(x, 3) == Q{Int}(3, length=1)
        @test div(10, y) == Q{Int}(3, mass=1)
        if VERSION >= v"1.9"
            @test div(x, y, RoundFromZero) == Q{Int}(4, length=1, mass=1)
            @test div(x, 3, RoundFromZero) == Q{Int}(4, length=1)
            @test div(10, y, RoundFromZero) == Q{Int}(4, mass=1)
        end
    end
end
