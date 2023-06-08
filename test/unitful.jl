import DynamicQuantities
import Unitful
import Unitful: @u_str
using Test

x = DynamicQuantities.Quantity(0.2, amount=2, current=-1 // 2, luminosity=2 // 5)
x_unitful = 0.2u"mol^2*A^(-1//2)*cd^(2//5)"

@test convert(Unitful.Quantity, x) ≈ x_unitful
@test convert(DynamicQuantities.Quantity, convert(Unitful.Quantity, x)) ≈ x

@test convert(DynamicQuantities.Quantity, u"c") ≈ 2.99792458e8 * DynamicQuantities.Dimensions(length=1, time=-1)
@test convert(DynamicQuantities.Dimensions, u"c") == DynamicQuantities.Dimensions(length=1, time=-1)
@test convert(DynamicQuantities.Dimensions, Unitful.dimension(u"c")) == DynamicQuantities.Dimensions(length=1, time=-1)

# Defining a custom dimension should throw an error:
Unitful.@dimension 𝚾 "𝚾" MyDimension
# @refunit m "m" Meter 𝐋 true
Unitful.@refunit my_unit "x" MyDimension 𝚾 true

@test_throws "Unknown dimension: MyDimension" convert(DynamicQuantities.Quantity, 1.0 * my_unit)
