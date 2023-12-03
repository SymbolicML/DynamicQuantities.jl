module Units

import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..DEFAULT_QUANTITY_TYPE

@assert DEFAULT_VALUE_TYPE == Float64 "`units.jl` must be updated to support a different default value type."

const _UNIT_SYMBOLS = Symbol[]
const _UNIT_VALUES = DEFAULT_QUANTITY_TYPE[]

macro register_unit(name, value)
    return esc(_register_unit(name, value))
end

macro add_prefixes(base_unit, prefixes)
    @assert prefixes.head == :tuple
    return esc(_add_prefixes(base_unit, prefixes.args, _register_unit))
end

function _register_unit(name::Symbol, value)
    s = string(name)
    return quote
        const $name = $value
        push!(_UNIT_SYMBOLS, Symbol($s))
        push!(_UNIT_VALUES, $name)
    end
end

function _add_prefixes(base_unit::Symbol, prefixes, register_function)
    all_prefixes = (
        f=1e-15, p=1e-12, n=1e-9, μ=1e-6, u=1e-6, m=1e-3, c=1e-2, d=1e-1,
        k=1e3, M=1e6, G=1e9, T=1e12
    )
    expr = Expr(:block)
    for (prefix, value) in zip(keys(all_prefixes), values(all_prefixes))
        prefix in prefixes || continue
        new_unit = Symbol(prefix, base_unit)
        push!(expr.args, register_function(new_unit, :($value * $base_unit)))
    end
    return expr
end

# SI base units
@register_unit m DEFAULT_QUANTITY_TYPE(1.0, length=1)
@register_unit g DEFAULT_QUANTITY_TYPE(1e-3, mass=1)
@register_unit s DEFAULT_QUANTITY_TYPE(1.0, time=1)
@register_unit A DEFAULT_QUANTITY_TYPE(1.0, current=1)
@register_unit K DEFAULT_QUANTITY_TYPE(1.0, temperature=1)
@register_unit cd DEFAULT_QUANTITY_TYPE(1.0, luminosity=1)
@register_unit mol DEFAULT_QUANTITY_TYPE(1.0, amount=1)

@add_prefixes m (f, p, n, μ, u, c, d, m, k, M, G)
@add_prefixes g (p, n, μ, u, m, k)
@add_prefixes s (f, p, n, μ, u, m)
@add_prefixes A (n, μ, u, m, k)
@add_prefixes K (m,)
@add_prefixes cd (m,)
@add_prefixes mol (m,)

@doc(
    "Length in meters. Available variants: `fm`, `pm`, `nm`, `μm` (/`um`), `cm`, `dm`, `mm`, `km`, `Mm`, `Gm`.",
    m,
)
@doc(
    "Mass in kilograms. Available variants: `pg`, `ng`, `μg` (/`ug`), `mg`, `g`.",
    kg,
)
@doc(
    "Time in seconds. Available variants: `fs`, `ps`, `ns`, `μs` (/`us`), `ms`, `min` (/`minute`), `h` (/`hr`), `day` (/`d`), `wk`, `yr`, `kyr`, `Myr`, `Gyr`.",
    s,
)
@doc(
    "Current in Amperes. Available variants: `nA`, `μA` (/`uA`), `mA`, `kA`.",
    A,
)
@doc(
    "Temperature in Kelvin. Available variant: `mK`.",
    K,
)
@doc(
    "Luminosity in candela. Available variant: `mcd`.",
    cd,
)
@doc(
    "Amount in moles. Available variant: `mmol`.",
    mol,
)

# SI derived units
@register_unit Hz inv(s)
@register_unit N kg * m / s^2
@register_unit Pa N / m^2
@register_unit J N * m
@register_unit W J / s
@register_unit C A * s
@register_unit V W / A
@register_unit F C / V
@register_unit Ω V / A
@register_unit ohm Ω
@register_unit T N / (A * m)

@add_prefixes Hz (k, M, G)
@add_prefixes N ()
@add_prefixes Pa (k,)
@add_prefixes J (k,)
@add_prefixes W (k, M, G)
@add_prefixes C ()
@add_prefixes V (m, k, M, G)
@add_prefixes F ()
@add_prefixes Ω (m,)
@add_prefixes ohm (m,)
@add_prefixes T ()

# SI derived units
@doc(
    "Frequency in Hertz. Available variants: `kHz`, `MHz`, `GHz`.",
    Hz,
)
@doc(
    "Force in Newtons.",
    N,
)
@doc(
    "Pressure in Pascals. Available variant: `kPa`.",
    Pa,
)
@doc(
    "Energy in Joules. Available variant: `kJ`.",
    J,
)
@doc(
    "Power in Watts. Available variants: `kW`, `MW`, `GW`.",
    W,
)
@doc(
    "Charge in Coulombs.",
    C,
)
@doc(
    "Voltage in Volts. Available variants: `kV`, `MV`, `GV`.",
    V,
)
@doc(
    "Capacitance in Farads.",
    F,
)
@doc(
    "Resistance in Ohms. Available variant: `mΩ`. Also available is ASCII `ohm` (with variant `mohm`).",
    Ω,
)
@doc(
    "Magnetic flux density in Teslas.",
    T,
)

# Common assorted units
## Time
@register_unit min 60 * s
@register_unit minute min
@register_unit h 60 * min
@register_unit hr h
@register_unit day 24 * h
@register_unit d day
@register_unit wk 7 * day
@register_unit yr 365.25 * day

@add_prefixes min ()
@add_prefixes minute ()
@add_prefixes h ()
@add_prefixes hr ()
@add_prefixes day ()
@add_prefixes d ()
@add_prefixes wk ()
@add_prefixes yr (k, M, G)

## Volume
@register_unit L dm^3

@add_prefixes L (m, c, d)

@doc(
    "Volume in liters. Available variants: `mL`, `cL`, `dL`.",
    L,
)

## Pressure
@register_unit bar 100 * kPa

@add_prefixes bar (m,)

@doc(
    "Pressure in bars. Available variants: `mbar`.",
    bar,
)

# Do not wish to define Gaussian units, as it changes
# some formulas. Safer to force user to work exclusively in one unit system.

# Do not wish to define physical constants, as the number of symbols might lead to ambiguity.
# The user should define these instead.

"""A tuple of all possible unit symbols."""
const UNIT_SYMBOLS = Tuple(_UNIT_SYMBOLS)
const UNIT_VALUES = Tuple(_UNIT_VALUES)
const UNIT_MAPPING = NamedTuple([s => i for (i, s) in enumerate(UNIT_SYMBOLS)])

end
