module DynamicQuantitiesMeasurementsExt

using DynamicQuantities: AbstractQuantity, new_quantity, dimension, ustrip, DimensionError
using Measurements: Measurements, measurement, value, uncertainty

function Measurements.measurement(a::Q, b::Q) where {Q<:AbstractQuantity}
    dimension(a) == dimension(b) || throw(DimensionError(a, b))
    raw_measurement = measurement(ustrip(a), ustrip(b))
    return new_quantity(Q, raw_measurement, dimension(a))
end
function Measurements.measurement(a::Q1, b::Q2) where {Q1<:AbstractQuantity, Q2<:AbstractQuantity}
    Q = promote_type(Q1, Q2)
    a = convert(Q, a)
    b = convert(Q, b)
    return measurement(a, b)
end

Measurements.value(q::Q) where {Q<:AbstractQuantity} = new_quantity(Q, value(ustrip(q)), dimension(q))
Measurements.uncertainty(q::Q) where {Q<:AbstractQuantity} = new_quantity(Q, uncertainty(ustrip(q)), dimension(q))

end
