using DynamicQuantities
using Measurements
using Measurements: value, uncertainty

x = 1.0u"m/s" ± 0.1u"m/s"

@test ustrip(x^2) == ustrip(x)^2
@test value(x) == 1.0u"m/s"
@test uncertainty(x) == 0.1u"m/s"
@test dimension(x)^2 == dimension(x^2)
@test_throws DimensionError 0.5u"m" ± 0.1u"s"

# Mixed types:
y = Quantity{Float16}(0.1u"m/s") ± Quantity{Float32}(0.1u"m/s")
@test typeof(y) <: Quantity{Measurement{Float32}}
