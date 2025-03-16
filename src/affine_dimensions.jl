"""
    AffineUnit{R}

A simple struct for representing affine units like Celsius and Fahrenheit.
This is not part of the AbstractDimensions hierarchy.
"""
struct AffineUnit{R}
    scale::Float64
    offset::Float64
    basedim::Dimensions{R}
end

# Define Celsius and Fahrenheit units
const CELSIUS = AffineUnit(1.0, 273.15, Dimensions{DEFAULT_DIM_BASE_TYPE}(temperature=1))
const FAHRENHEIT = AffineUnit(5/9, 459.67 * 5/9, Dimensions{DEFAULT_DIM_BASE_TYPE}(temperature=1))

# This immediately converts to regular Dimensions
function Base.:*(value::Number, unit::AffineUnit)
    # Apply the affine transformation: value * scale + offset
    new_value = value * unit.scale + unit.offset
    # Always use Float64 for temperature conversions to avoid precision issues
    return Quantity(new_value, unit.basedim)
end

# String parsing for affine units
"""
    ua"unit"

Parse a string containing an affine unit expression.
Currently only supports °C and °F.
"""
macro ua_str(s)
    return esc(:(aff_uparse($s)))
end

# For compatibility with existing code
function aff_uparse(s::AbstractString)
    if s == "°C" || s == "degC"
        return CELSIUS
    elseif s == "°F" || s == "degF"
        return FAHRENHEIT
    else
        # For other units, convert to regular units
        return uparse(s)
    end
end
