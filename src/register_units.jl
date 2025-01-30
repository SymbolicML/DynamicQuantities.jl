import .Units: UNIT_MAPPING, UNIT_SYMBOLS, UNIT_VALUES, _lazy_register_unit
import .SymbolicUnits: update_external_symbolic_unit_value

# Update the unit collections
const UNIT_UPDATE_LOCK = Threads.SpinLock()

function update_all_values(name_symbol, unit)
    lock(UNIT_UPDATE_LOCK) do
        push!(ALL_SYMBOLS, name_symbol)
        push!(ALL_VALUES, unit)
        i = lastindex(ALL_VALUES)
        ALL_MAPPING[name_symbol] = i
        UNIT_MAPPING[name_symbol] = i
        update_external_symbolic_unit_value(name_symbol)
        update_external_affine_unit(name_symbol, unit)
    end
end

function update_affine_values(name_symbol, unit)
    lock(UNIT_UPDATE_LOCK) do
        update_external_affine_unit(name_symbol, unit)
    end
end


"""
    @register_unit symbol value

Register a new unit under the given symbol to have
a particular value.

# Example

```julia
julia> @register_unit MyVolt 1.5u"V"
```

This will register a new unit `MyVolt` with a value of `1.5u"V"`.
You can then use this unit in your calculations:

```julia
julia> x = 20us"MyVolt^2"
20.0 MyVolt²

julia> y = 2.5us"A"
2.5 A

julia> x * y^2 |> us"W^2"
281.25 W²

julia> x * y^2 |> us"W^2" |> sqrt |> uexpand
16.77050983124842 m² kg s⁻³
```

"""
macro register_unit(symbol, value)
    return esc(_register_unit(symbol, value))
end

function _register_unit(name::Symbol, value)
    name_symbol = Meta.quot(name)
    index = get(ALL_MAPPING, name, INDEX_TYPE(0))
    if !iszero(index)
        unit = ALL_VALUES[index]
        # When a utility function to expand `value` to its final form becomes
        # available, enable the following check. This will avoid throwing an error
        # if user is trying to register an existing unit with matching values.
        # unit.value != value && throw("Unit $name is already defined as $unit")
        error("Unit `$name` is already defined as `$unit`")
    end
    reg_expr = _lazy_register_unit(name, value)
    push!(
        reg_expr.args,
        quote
            $update_all_values($name_symbol, $value)
            nothing
        end
    )
    return reg_expr
end


macro register_affine_unit(name, expr)
    return esc(_register_affine_unit(name, expr))
end

function _register_affine_unit(name, expr)
    name_symbol = Meta.quot(name)
    index = get(AffineUnitsParse.AFFINE_UNIT_MAPPING, name, INDEX_TYPE(0))
    if !iszero(index)
        unit = AffineUnitsParse.AFFINE_UNIT_VALUES[index]
        error("Unit `$name` is already defined as `$unit`")
    end
    return :($update_affine_values($name_symbol, $expr))
end