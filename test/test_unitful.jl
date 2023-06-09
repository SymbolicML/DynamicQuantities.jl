import DynamicQuantities
import Unitful
import Unitful: @u_str
import Ratios: SimpleRatio
import SaferIntegers: SafeInt16
using Test

# Try to work with different preferred units:
Unitful.preferunits(u"km")

risapprox(x::Unitful.Quantity, y::Unitful.Quantity; kws...) =
    let (xfloat, yfloat) = (Unitful.ustrip ∘ Unitful.upreferred).((x, y))
        return isapprox(xfloat, yfloat; kws...)
    end

factor_for_preferred_units = 1e-3

for T in [Float16, Float32, Float64], R in [Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
    x = DynamicQuantities.Quantity(T(0.2*factor_for_preferred_units), R, length=1, amount=2, current=-1 // 2, luminosity=2 // 5)
    x_unitful = T(0.2)u"m*mol^2*A^(-1//2)*cd^(2//5)"

    @test risapprox(convert(Unitful.Quantity, x), x_unitful; atol=1e-6)
    @test typeof(convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x))) <: DynamicQuantities.Quantity{T,DynamicQuantities.DEFAULT_DIM_TYPE}
    @test isapprox(convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x)), x; atol=1e-6)

    @test isapprox(convert(DynamicQuantities.Quantity{T,R}, x_unitful), x; atol=1e-6)
    @test risapprox(convert(Unitful.Quantity, convert(DynamicQuantities.Quantity{T,R}, x_unitful)), Unitful.upreferred(x_unitful); atol=1e-6)
end
