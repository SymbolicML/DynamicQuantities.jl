using DynamicQuantities
using Measurements
using Measurements: value, uncertainty

for Q in (Quantity, GenericQuantity)
    x = Q(1.0u"m/s") ± Q(0.1u"m/s")

    @test ustrip(x^2) == ustrip(x)^2
    @test value(x) == 1.0u"m/s"
    @test uncertainty(x) == 0.1u"m/s"
    @test dimension(x)^2 == dimension(x^2)
    @test_throws DimensionError 0.5u"m" ± 0.1u"s"

    # Mixed types:
    y = Q{Float16}(0.1u"m/s") ± Q{Float32}(0.1u"m/s")
    @test typeof(y) <: Q{Measurement{Float32}}
end
