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
        f=1e-15, p=1e-12, n=1e-9, μ=1e-6, u=1e-6, m=1e-3, c=1e-2,
        k=1e3, M=1e6, G=1e9, T=1e12, P=1e15
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
const m = Quantity(1.0, length=1)
const g = Quantity(1e-3, mass=1)
const s = Quantity(1.0, time=1)
const A = Quantity(1.0, current=1)
const K = Quantity(1.0, temperature=1)
const cd = Quantity(1.0, luminosity=1)
const mol = Quantity(1.0, amount=1)

@add_prefixes m (f, p, n, μ, u, c, m, k, M, G)
@add_prefixes g (μ, u, m, k)
@add_prefixes s (f, p, n, μ, u, m)
@add_prefixes A (n, μ, u, m, k)
@add_prefixes K (m,)
@add_prefixes cd (m,)
@add_prefixes mol (m,)

# SI derived units
const Hz = inv(s)
const N = kg * m / s^2
const Pa = N / m^2
const J = N * m
const W = J / s
const C = A * s
const V = W / A
const F = C / V
const Ω = V / A
const T = N / (A * m)

@add_prefixes Hz (k, M, G)
@add_prefixes N ()
@add_prefixes P ()
@add_prefixes J ()
@add_prefixes W (k, M, G)
@add_prefixes C ()
@add_prefixes V (m, k, M, G)
@add_prefixes F ()
@add_prefixes Ω (m,)
@add_prefixes T ()

# Assorted units
const min = 60 * s
const hour = 60 * min
const day = 24 * hour
const wk = 7 * day
const yr = 365.25 * day

@add_prefixes yr (k, M, G)

# Do not wish to define Gaussian units, as it changes
# some formulas. Safer to force user to work exclusively in one unit system.

# Do not wish to define physical constants, as the number of symbols might lead to ambiguity.
# The user should define these instead.

"""
    uparse(s::AbstractString)

Parse a string containing an expression of units and return the
corresponding `Quantity` object. For example, `uparse("m/s")`
would be parsed to `Quantity(1.0, length=1, time=-1)`.
"""
function uparse(s::AbstractString)
    return eval(Meta.parse(s))::Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}
end

"""
    @u_str(s::AbstractString)

Parse a string containing an expression of units and return the
corresponding `Quantity` object. For example, `q"km/s^2"`
would be parsed to `Quantity(1000.0, length=1, time=-2)`.
"""
macro u_str(s)
    return esc(uparse(s))
end

end