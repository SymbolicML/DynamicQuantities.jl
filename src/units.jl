module Units

import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..Quantity

@assert DEFAULT_VALUE_TYPE == Float64 "`units.jl` must be updated to support a different default value type."

macro add_prefixes(base_unit, prefixes)
    @assert prefixes.head == :tuple
    expr = _add_prefixes(base_unit, prefixes.args)
    return expr |> esc
end

function _add_prefixes(base_unit::Symbol, prefixes)
    all_prefixes = (
        f=1e-15, p=1e-12, n=1e-9, μ=1e-6, u=1e-6, m=1e-3, c=1e-2, d=1e-1,
        k=1e3, M=1e6, G=1e9, T=1e12
    )
    expr = Expr(:block)
    for (prefix, value) in zip(keys(all_prefixes), values(all_prefixes))
        prefix in prefixes || continue
        new_unit = Symbol(prefix, base_unit)
        push!(expr.args, :(const $new_unit = $value * $base_unit))
    end
    return expr
end

# SI base units
"Length in meters. Available variants: `fm`, `pm`, `nm`, `μm` (/`um`), `cm`, `dm`, `mm`, `km`, `Mm`, `Gm`."
const m = Quantity(1.0, length=1)
"Mass in grams. Available variants: `μg` (/`ug`), `mg`, `kg`."
const g = Quantity(1e-3, mass=1)
"Time in seconds. Available variants: `fs`, `ps`, `ns`, `μs` (/`us`), `ms`, `min`, `h` (/`hr`), `day`, `yr`, `kyr`, `Myr`, `Gyr`."
const s = Quantity(1.0, time=1)
"Current in Amperes. Available variants: `nA`, `μA` (/`uA`), `mA`, `kA`."
const A = Quantity(1.0, current=1)
"Temperature in Kelvin. Available variant: `mK`."
const K = Quantity(1.0, temperature=1)
"Luminosity in candela. Available variant: `mcd`."
const cd = Quantity(1.0, luminosity=1)
"Amount in moles. Available variant: `mmol`."
const mol = Quantity(1.0, amount=1)

@add_prefixes m (f, p, n, μ, u, c, d, m, k, M, G)
@add_prefixes g (μ, u, m, k)
@add_prefixes s (f, p, n, μ, u, m)
@add_prefixes A (n, μ, u, m, k)
@add_prefixes K (m,)
@add_prefixes cd (m,)
@add_prefixes mol (m,)

# SI derived units
"Frequency in Hertz. Available variants: `kHz`, `MHz`, `GHz`."
const Hz = inv(s)
"Force in Newtons."
const N = kg * m / s^2
"Pressure in Pascals. Available variant: `kPa`."
const Pa = N / m^2
"Energy in Joules. Available variant: `kJ`."
const J = N * m
"Power in Watts. Available variants: `kW`, `MW`, `GW`."
const W = J / s
"Charge in Coulombs."
const C = A * s
"Voltage in Volts. Available variants: `kV`, `MV`, `GV`."
const V = W / A
"Capacitance in Farads."
const F = C / V
"Resistance in Ohms. Available variant: `mΩ`. Also available is ASCII `ohm` (with variant `mohm`)."
const Ω = V / A
const ohm = Ω
"Magnetic flux density in Teslas."
const T = N / (A * m)

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

# Common assorted units
## Time
const min = 60 * s
const h = 60 * min
const hr = h
const day = 24 * h
const yr = 365.25 * day

@add_prefixes min ()
@add_prefixes h ()
@add_prefixes hr ()
@add_prefixes day ()
@add_prefixes yr (k, M, G)

## Volume
"Volume in liters. Available variants: `mL`, `dL`."
const L = dm^3

@add_prefixes L (m, d)

## Pressure
"Pressure in bars."
const bar = 100 * kPa

@add_prefixes bar ()

# Do not wish to define Gaussian units, as it changes
# some formulas. Safer to force user to work exclusively in one unit system.

# Do not wish to define physical constants, as the number of symbols might lead to ambiguity.
# The user should define these instead.

end
