using DynamicUnits
using Test

x = Quantity(0.2, length=1, mass=2.5)

@test x.length === 1 // 1
@test x.mass === 5 // 2
@test x.value === 0.2
@test x.valid === true
@test typeof(x).parameters[1] == Float64

y = x^2

@test y.length === 2 // 1
@test y.mass === 5 // 1
@test y.value ≈ 0.04
@test typeof(y).parameters[1] == Float64