import DynamicUnits
import Unitful
import Unitful: @u_str
using Test

x = DynamicUnits.Quantity(0.2, amount=2, current=-1 // 2, luminosity=2 // 5)
x_unitful = 0.2u"mol^2*A^(-1//2)*cd^(2//5)"

@test convert(Unitful.Quantity, x) ≈ x_unitful
@test convert(DynamicUnits.Quantity, convert(Unitful.Quantity, x)) ≈ x
