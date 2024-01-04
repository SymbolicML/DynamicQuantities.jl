module UnitsParse

import ..constructorof
import ..DEFAULT_QUANTITY_TYPE
import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..Units: UNIT_SYMBOLS, UNIT_VALUES
import ..Constants: CONSTANT_SYMBOLS, CONSTANT_VALUES
import ..Constants

function _generate_units_import()
    import_expr = :(import ..Units: _)
    deleteat!(first(import_expr.args).args, 2)
    for symb in UNIT_SYMBOLS
        push!(first(import_expr.args).args, Expr(:., symb))
    end
    return import_expr
end
macro generate_units_import()
    return _generate_units_import()
end

@generate_units_import

"""
    uparse(s::AbstractString)

Parse a string containing an expression of units and return the
corresponding `Quantity` object with `Float64` value. For example,
`uparse("m/s")` would be parsed to `Quantity(1.0, length=1, time=-1)`.

Note that inside this expression, you also have access to the `Constants`
module. So, for example, `uparse("Constants.c^2 * Hz^2")` would evaluate to
the quantity corresponding to the speed of light multiplied by Hertz,
squared.
"""
function uparse(s::AbstractString)
    return as_quantity(eval(map_to_scope(Meta.parse(s))))::DEFAULT_QUANTITY_TYPE
end

as_quantity(q::DEFAULT_QUANTITY_TYPE) = q
as_quantity(x::Number) = convert(DEFAULT_QUANTITY_TYPE, x)
as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")

"""
    u"[unit expression]"

Parse a string containing an expression of units and return the
corresponding `Quantity` object with `Float64` value. For example,
`u"km/s^2"` would be parsed to `Quantity(1000.0, length=1, time=-2)`.

Note that inside this expression, you also have access to the `Constants`
module. So, for example, `u"Constants.c^2 * Hz^2"` would evaluate to
the quantity corresponding to the speed of light multiplied by Hertz,
squared.
"""
macro u_str(s)
    ex = Meta.parse(s)
    return esc(map_to_scope(ex))
end

function map_to_scope(ex::Expr)
    if ex.head == :call
        ex.args[2:end] = map(map_to_scope, ex.args[2:end])
        return ex
    elseif ex.head == :tuple
        ex.args[:] = map(map_to_scope, ex.args)
        return ex
    elseif ex.head == :.
        if ex.args[1] == :Constants
            @assert ex.args[2] isa QuoteNode
            return lookup_constant(ex.args[2].value)
        else
            return ex
        end
    else
        throw(ArgumentError("Unexpected expression: $ex. Only `:call`, `:tuple`, and `:.` are expected."))
        return ex
    end
end
function map_to_scope(sym::Symbol)
    if sym in UNIT_SYMBOLS
        return lookup_unit(sym)
    elseif sym in CONSTANT_SYMBOLS
        throw(ArgumentError("Symbol $sym found in `Constants` but not `Units`. Please access the `Constants` module. For example, `u\"Constants.$sym\"`."))
        return sym
    else
        throw(ArgumentError("Symbol $sym not found in `Units` or `Constants`."))
        return sym
    end
end
function map_to_scope(ex)
    return ex
end
function lookup_unit(ex::Symbol)
    i = findfirst(==(ex), UNIT_SYMBOLS)::Int
    return UNIT_VALUES[i]
end
function lookup_constant(ex::Symbol)
    i = findfirst(==(ex), CONSTANT_SYMBOLS)::Int
    return CONSTANT_VALUES[i]
end

end
