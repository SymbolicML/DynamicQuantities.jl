import DynamicQuantities
using DynamicQuantities: DEFAULT_DIM_BASE_TYPE, DEFAULT_VALUE_TYPE
import Unitful
import Unitful: @u_str
import Ratios: SimpleRatio
import SaferIntegers: SafeInt16
using Test

function risapprox(x::Unitful.Quantity, y::Unitful.Quantity; kws...)
    (xfloat, yfloat) = (Unitful.ustrip ∘ Unitful.upreferred).((x, y))
    return isapprox(xfloat, yfloat; kws...)
end

for T in [DEFAULT_VALUE_TYPE, Float16, Float32, Float64], R in [DEFAULT_DIM_BASE_TYPE, Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
    D = DynamicQuantities.Dimensions{R}
    x = DynamicQuantities.Quantity(T(0.2), D, length=1, amount=2, current=-1 // 2, luminosity=2 // 5)
    x_unitful = T(0.2)u"m*mol^2*A^(-1//2)*cd^(2//5)"

    @test risapprox(convert(Unitful.Quantity, x), x_unitful; atol=1e-6)
    @test typeof(convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x))) <: DynamicQuantities.Quantity{T,DynamicQuantities.DEFAULT_DIM_TYPE}
    @test isapprox(convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x)), x; atol=1e-6)

    @test isapprox(convert(DynamicQuantities.Quantity{T,D}, x_unitful), x; atol=1e-6)
    @test risapprox(convert(Unitful.Quantity, convert(DynamicQuantities.Quantity{T,D}, x_unitful)), Unitful.upreferred(x_unitful); atol=1e-6)

    @test typeof(convert(DynamicQuantities.Dimensions, Unitful.dimension(x_unitful))) == DynamicQuantities.Dimensions{DEFAULT_DIM_BASE_TYPE}
end

module MyScaleUnit
    using Unitful
    @dimension(𝐒, "𝐒", Scale)
    @refunit(scale, "scale", Scale, 𝐒, false)
end

Unitful.register(MyScaleUnit)

x = 1.0u"scale"
@test typeof(x) <: Unitful.Quantity{Float64, MyScaleUnit.𝐒}
@test_throws ErrorException convert(DynamicQuantities.Quantity, x)
# These are not supported because there is no SI equivalency

# issue 79
symbolic = DynamicQuantities.us"s"
@test_throws ArgumentError convert(Unitful.Quantity, symbolic)
@test convert(Unitful.Quantity, DynamicQuantities.uexpand(symbolic)) == 1.0 * Unitful.u"s"
