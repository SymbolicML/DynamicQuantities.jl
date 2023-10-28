module DynamicQuantitiesMeasurementsExt

using DynamicQuantities: AbstractUnionQuantity, new_quantity, dimension, ustrip, DimensionError
using Measurements: Measurements, measurement, value, uncertainty

function Measurements.measurement(a::Q, b::Q) where {Q<:AbstractUnionQuantity}
    dimension(a) == dimension(b) || throw(DimensionError(a, b))
    raw_measurement = measurement(ustrip(a), ustrip(b))
    return new_quantity(Q, raw_measurement, dimension(a))
end
function Measurements.measurement(a::AbstractUnionQuantity, b::AbstractUnionQuantity)
    return measurement(promote(a, b)...)
end

Measurements.value(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, value(ustrip(q)), dimension(q))
Measurements.uncertainty(q::Q) where {Q<:AbstractUnionQuantity} = new_quantity(Q, uncertainty(ustrip(q)), dimension(q))

end
