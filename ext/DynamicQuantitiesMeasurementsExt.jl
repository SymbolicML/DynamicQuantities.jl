module DynamicQuantitiesMeasurementsExt

using DynamicQuantities: UnionAbstractQuantity, new_quantity, dimension, ustrip, DimensionError
using Measurements: Measurements, measurement, value, uncertainty

function Measurements.measurement(a::Q, b::Q) where {Q<:UnionAbstractQuantity}
    dimension(a) == dimension(b) || throw(DimensionError(a, b))
    raw_measurement = measurement(ustrip(a), ustrip(b))
    return new_quantity(Q, raw_measurement, dimension(a))
end
function Measurements.measurement(a::UnionAbstractQuantity, b::UnionAbstractQuantity)
    return measurement(promote(a, b)...)
end

Measurements.value(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, value(ustrip(q)), dimension(q))
Measurements.uncertainty(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, uncertainty(ustrip(q)), dimension(q))

end
