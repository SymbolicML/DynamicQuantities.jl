module UnitsParse

import ..Quantity
import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..Units: UNIT_SYMBOLS
import ..Constants

function _generate_units_import()
    import_expr = :(import ..Units: _)
    deleteat!(first(import_expr.args).args, 2)
    for symb in UNIT_SYMBOLS
        push!(first(import_expr.args).args, Expr(:., symb))
    end
    return import_expr
end

eval(_generate_units_import())

"""
    uparse(s::AbstractString)

Parse a string containing an expression of units and return the
corresponding `Quantity` object with `Float64` value. For example,
`uparse("m/s")` would be parsed to `Quantity(1.0, length=1, time=-1)`.
"""
function uparse(s::AbstractString)
    return as_quantity(eval(Meta.parse(s)))::Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
end

as_quantity(q::Quantity) = q
as_quantity(x::Number) = Quantity(convert(DEFAULT_VALUE_TYPE, x), DEFAULT_DIM_TYPE)
as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")

"""
    u"[unit expression]"

Parse a string containing an expression of units and return the
corresponding `Quantity` object with `Float64` value. For example,
`u"km/s^2"` would be parsed to `Quantity(1000.0, length=1, time=-2)`.
"""
macro u_str(s)
    return esc(uparse(s))
end

end