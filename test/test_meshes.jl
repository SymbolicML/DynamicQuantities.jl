using DynamicQuantities
using Meshes: Meshes

v = Meshes.Vec(1.0u"m/s", 2.0u"m/s", 3.0u"m/s")
@test typeof(v) <: Meshes.Vec{3, <:Quantity{Float64, <:Dimensions}}
@test v.coords == (1.0u"m/s", 2.0u"m/s", 3.0u"m/s")
