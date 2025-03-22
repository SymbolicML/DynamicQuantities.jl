"""
    AffineUnit{R}

A simple struct for representing affine units like Celsius and Fahrenheit.
This is not part of the AbstractDimensions hierarchy.

AffineUnit only supports scalar multiplication in the form `number * unit` (e.g., `22ua"degC"`),
which immediately converts it to a regular `Quantity{Float64,Dimensions{R}}`. Other operations
like `unit * number`, division, addition, or subtraction with AffineUnit are not supported.

!!! warning "Non-associative multiplication"
    Multiplication with AffineUnit is non-associative due to the auto-conversion property.
    For example, `(2 * 3) * ua"degC"` ≠ `2 * (3 * ua"degC")` because when a number multiplies an AffineUnit,
    it immediately converts to a regular Quantity with the affine transformation applied.

!!! warning
    This is an experimental feature and may change in the future.
"""
struct AffineUnit{R}
    scale::Float64
    offset::Float64
    basedim::Dimensions{R}
    name::Symbol
end

Base.show(io::IO, unit::AffineUnit) = print(io, unit.name)

# This immediately converts to regular Dimensions
function Base.:*(value::Number, unit::AffineUnit)
    # Apply the affine transformation: value * scale + offset
    new_value = value * unit.scale + unit.offset
    # Always use Float64 for temperature conversions to avoid precision issues
    return Quantity(new_value, unit.basedim)
end

# Error messages for unsupported operations - defined using a loop
for op in [:*, :/, :+, :-], (first, second) in [(:AffineUnit, :Number), (:Number, :AffineUnit)]

    # Skip the already defined value * unit case
    op == :* && first == :Number && second == :AffineUnit && continue
    
    @eval function Base.$op(a::$first, b::$second)
        throw(ArgumentError("Affine units only support scalar multiplication in the form 'number * unit', e.g., 22 * ua\"degC\", which will immediately convert it to a regular `Quantity{Float64,Dimensions{R}}`. Other operations are not supported."))
    end
end

# Module for affine unit parsing
module AffineUnits
    import ..AffineUnit
    import ..Dimensions
    import ..DEFAULT_DIM_BASE_TYPE
    import ..Quantity

    # Define Celsius and Fahrenheit units inside the module
    const °C = AffineUnit(1.0, 273.15, Dimensions{DEFAULT_DIM_BASE_TYPE}(temperature=1), :°C)
    const degC = °C
    const °F = AffineUnit(5/9, 459.67 * 5/9, Dimensions{DEFAULT_DIM_BASE_TYPE}(temperature=1), :°F)
    const degF = °F

    const AFFINE_UNIT_SYMBOLS = [:°C, :degC, :°F, :degF]

    function map_to_scope(ex::Expr)
        if ex.head != :call
            throw(ArgumentError("Unexpected expression: $ex. Only `:call` is expected."))
        end
        ex.args[2:end] = map(map_to_scope, ex.args[2:end])
        return ex
    end

    function map_to_scope(sym::Symbol)
        if !(sym in AFFINE_UNIT_SYMBOLS)
            throw(ArgumentError("Symbol $sym not found in affine units. Only °C/degC and °F/degF are supported."))
        end
        if sym in (:°C, :degC)
            return °C
        else  # if sym in (:°F, :degF)
            return °F
        end
    end

    # For literals and other expressions
    map_to_scope(ex) = ex
end

"""
    ua"unit"

Parse a string containing an affine unit expression.
Currently only supports °C (or degC) and °F (or degF).

For example:

```julia
room_temp = 22ua"degC"  # The multiplication returns a Quantity
```

!!! warning
    This is an experimental feature and may change in the future.
"""
macro ua_str(s)
    ex = AffineUnits.map_to_scope(Meta.parse(s))
    return esc(ex)
end

"""
    aff_uparse(s::AbstractString)

Parse a string into an affine unit (°C/degC, °F/degF). Function equivalent of `ua"unit"`.

!!! warning
    This is an experimental feature and may change in the future.
"""
function aff_uparse(s::AbstractString)
    ex = AffineUnits.map_to_scope(Meta.parse(s))
    return eval(ex)::AffineUnit{DEFAULT_DIM_BASE_TYPE}
end
