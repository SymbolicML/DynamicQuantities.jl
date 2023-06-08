import DynamicQuantities
import Unitful
import Unitful: @u_str
import Ratios: SimpleRatio
import SaferIntegers: SafeInt16
using Test

# Try to work with different preferred units:
Unitful.preferunits(u"mA")
Unitful.preferunits(u"km")

for T in [Float16, Float32, Float64], R in [Rational{Int16}, Rational{Int32}, SimpleRatio{Int}, SimpleRatio{SafeInt16}]
    x = DynamicQuantities.Quantity(T(0.2), R, length=1, amount=2, current=-1 // 2, luminosity=2 // 5)
    x_unitful = T(0.2)u"km*mol^2*A^(-1//2)*cd^(2//5)"

    @test convert(Unitful.Quantity, x) ≈ x_unitful
    @test typeof(convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x))) <: DynamicQuantities.Quantity{T,DynamicQuantities.DEFAULT_DIM_TYPE}
    @test convert(DynamicQuantities.Quantity{T,R}, convert(Unitful.Quantity, x)) ≈ x
end
