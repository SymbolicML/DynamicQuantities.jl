import .Units: UNIT_MAPPING, UNIT_SYMBOLS, UNIT_VALUES, _lazy_register_unit
import .SymbolicUnits:
    SymbolicDimensionsSingleton, SYMBOLIC_UNIT_VALUES, update_symbolic_unit_values!

# Update the unit collections
function update_unit_mapping(name, value, unit_mapping::Dict{Symbol,Int} = UNIT_MAPPING)
    unit_mapping[name] = length(unit_mapping) + 1
end

function update_all_values(name_symbol, unit)
    push!(ALL_SYMBOLS, name_symbol)
    push!(ALL_VALUES, unit)
    ALL_MAPPING[name_symbol] = INDEX_TYPE(length(ALL_MAPPING) + 1)
end

# Register
macro register_unit(name, value)
    return esc(_register_unit(name, value))
end

function _register_unit(name::Symbol, value)
    name_symbol = Meta.quot(name)
    reg_expr = _lazy_register_unit(name, value)
    push!(reg_expr.args,
        quote
            $update_unit_mapping($name_symbol, $value)
            $update_all_values($name_symbol, $value)
            $update_symbolic_unit_values!($name_symbol)
            # suppress the print of `SYMBOLIC_UNIT_VALUES`
            nothing
        end
    )
    return reg_expr
end

