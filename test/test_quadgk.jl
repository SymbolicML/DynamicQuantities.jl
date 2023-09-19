using DynamicQuantities
using Test
using QuadGK

integral = quadgk(t -> 5u"m/s^2" * t, 0u"s", 10u"s")

@test integral == (5u"m/s^2" * (10u"s")^2 / 2, 0.0u"m")
