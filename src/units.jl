module Units

import ..WriteOnceReadMany
import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..DEFAULT_QUANTITY_TYPE

@assert DEFAULT_VALUE_TYPE == Float64 "`units.jl` must be updated to support a different default value type."

const UNIT_SYMBOLS = WriteOnceReadMany{Vector{Symbol}}()
const UNIT_VALUES = WriteOnceReadMany{Vector{DEFAULT_QUANTITY_TYPE}}()

macro _lazy_register_unit(name, value)
    return esc(_lazy_register_unit(name, value))
end

macro add_prefixes(base_unit, prefixes)
    @assert prefixes.head == :tuple
    return esc(_add_prefixes(base_unit, prefixes.args, _lazy_register_unit))
end

function _lazy_register_unit(name::Symbol, value)
    name_symbol = Meta.quot(name)
    quote
        const $name = $value
        push!($UNIT_SYMBOLS, $name_symbol)
        push!($UNIT_VALUES, $name)
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
@_lazy_register_unit m DEFAULT_QUANTITY_TYPE(1.0, length=1)
@_lazy_register_unit g DEFAULT_QUANTITY_TYPE(1e-3, mass=1)
@_lazy_register_unit s DEFAULT_QUANTITY_TYPE(1.0, time=1)
@_lazy_register_unit A DEFAULT_QUANTITY_TYPE(1.0, current=1)
@_lazy_register_unit K DEFAULT_QUANTITY_TYPE(1.0, temperature=1)
@_lazy_register_unit cd DEFAULT_QUANTITY_TYPE(1.0, luminosity=1)
@_lazy_register_unit mol DEFAULT_QUANTITY_TYPE(1.0, amount=1)

@add_prefixes m (f, p, n, μ, u, c, d, m, k, M, G)
@add_prefixes g (p, n, μ, u, m, k)
@add_prefixes s (f, p, n, μ, u, m)
@add_prefixes A (n, μ, u, m, k)
@add_prefixes K (m,)
@add_prefixes cd (m,)
@add_prefixes mol (p, n, μ, u, m)

@doc(
    "Length in meters. Available variants: `fm`, `pm`, `nm`, `μm` (/`um`), `cm`, `inch`, `dm`, `mm`, `ft`, `km`, `mi`, `Mm`, `Gm`.",
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
    "Amount in moles. Available variant: `pmol`, `nmol`, `μmol` (/`umol`), `mmol`.",
    mol,
)

# SI derived units
@_lazy_register_unit Hz inv(s)
@_lazy_register_unit N kg * m / s^2
@_lazy_register_unit Pa N / m^2
@_lazy_register_unit J N * m
@_lazy_register_unit W J / s
@_lazy_register_unit C A * s
@_lazy_register_unit V W / A
@_lazy_register_unit F C / V
@_lazy_register_unit Ω V / A
@_lazy_register_unit ohm Ω
@_lazy_register_unit S A / V
@_lazy_register_unit H N * m / A^2
@_lazy_register_unit T N / (A * m)
@_lazy_register_unit Wb V * s

@add_prefixes Hz (n, μ, u, m, k, M, G)
@add_prefixes N ()
@add_prefixes Pa (k,)
@add_prefixes J (k,)
@add_prefixes W (m, k, M, G)
@add_prefixes C ()
@add_prefixes V (p, n, μ, u, m, k, M, G)
@add_prefixes F (f, p, n, μ, u, m)
@add_prefixes Ω (n, μ, u, m, k, M, G)
@add_prefixes ohm (n, μ, u, m, k, M, G)
@add_prefixes S (n, μ, u, m, k, M, G)
@add_prefixes H ()
@add_prefixes T ()
@add_prefixes Wb (n, μ, u, m)

# SI derived units
@doc(
    "Frequency in Hertz. Available variants: `nHz`, `μHz` (/`uHz`), `mHz`, `kHz`, `MHz`, `GHz`, `rpm`.",
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
    "Power in Watts. Available variants: `mW`, `kW`, `MW`, `GW`.",
    W,
)
@doc(
    "Charge in Coulombs.",
    C,
)
@doc(
    "Voltage in Volts. Available variants: `pV`, `nV`, `μV` (/`uV`), `mV`, kV`, `MV`, `GV`.",
    V,
)
@doc(
    "Capacitance in Farads. Available variants: `fF`, `pF`, `nF`, `μF` (/`uF`), `mF`.",
    F,
)
@doc(
    "Resistance in Ohms. Available variant: `nΩ`, `μΩ` (/`uΩ`), `mΩ`, `kΩ`, `MΩ`, `GΩ`. Also available is ASCII `ohm` (with variants `nohm`, `μohm` (/`uohm`), `mohm`, `kohm`, `Mohm`, `Gohm`).",
    Ω,
)
@doc(
    "Electrical conductance, electric susceptance, and electric admittance in siemens. Available variants: `nS`, `μS` (/`uS`), `mS`, `kS`, `MS`, `GS`.",
    S,
)
@doc(
    "Electrical inductance in henries.",
    H,
)
@doc(
    "Magnetic flux density in Teslas.",
    T,
)
@doc(
    "Magnetic flux in webers. Available variants: `nWb`, `μWb` (/`uWb`), `mWb`.",
    Wb,
)

# Common assorted units
## Time
@_lazy_register_unit min 60 * s
@_lazy_register_unit minute min
@_lazy_register_unit h 60 * min
@_lazy_register_unit hr h
@_lazy_register_unit day 24 * h
@_lazy_register_unit d day
@_lazy_register_unit wk 7 * day
@_lazy_register_unit yr 365.25 * day
@_lazy_register_unit inch 2.54 * cm
@_lazy_register_unit ft 12 * inch
@_lazy_register_unit mi 5280 * ft

@add_prefixes min ()
@add_prefixes minute ()
@add_prefixes h ()
@add_prefixes hr ()
@add_prefixes day ()
@add_prefixes d ()
@add_prefixes wk ()
@add_prefixes yr (k, M, G)

## Volume
@_lazy_register_unit L dm^3

@add_prefixes L (μ, u, m, c, d)

@doc(
    "Volume in liters. Available variants: `μL` (/`uL`), `mL`, `cL`, `dL`.",
    L,
)

## Pressure
@_lazy_register_unit bar 100 * kPa

@add_prefixes bar (m,)

@doc(
    "Pressure in bars. Available variants: `mbar`.",
    bar,
)

## Angles
@_lazy_register_unit rad DEFAULT_QUANTITY_TYPE(1.0)
@_lazy_register_unit sr DEFAULT_QUANTITY_TYPE(1.0)

@add_prefixes rad (n, μ, u, m)
@add_prefixes sr ()

@doc(
    "Angle in radians. Note that the SI definition is simply 1 rad = 1, so use symbolic units to avoid this. Available variants: `nrad`, `μrad` (/`urad`), `mrad`, `deg`, `arcmin`, `arcsec`, `μarcsec` (/`uarcsec`), `marcsec`.",
    rad,
)
@doc(
    "Solid angle in steradians. Note that the SI definition is simply 1 sr = 1, so use symbolic units to avoid this.",
    sr,
)

@_lazy_register_unit deg pi / 180 * rad
@_lazy_register_unit arcmin deg / 60
@_lazy_register_unit arcsec arcmin / 60

@add_prefixes deg ()
@add_prefixes arcmin ()
@add_prefixes arcsec (μ, u, m)

## Angular velocity
@_lazy_register_unit rpm 2 * pi / min

# Do not wish to define Gaussian units, as it changes
# some formulas. Safer to force user to work exclusively in one unit system.

# Do not wish to define physical constants, as the number of symbols might lead to ambiguity.
# The user should define these instead.

# Update `UNIT_MAPPING` with all internally defined unit symbols.
const UNIT_MAPPING = WriteOnceReadMany(Dict(s => i for (i, s) in enumerate(UNIT_SYMBOLS)))

end
