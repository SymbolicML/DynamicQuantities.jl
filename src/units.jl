module Units

export uparse, @u_str

import SparseArrays as SA
import Tricks: static_fieldnames

import ..DEFAULT_DIM_TYPE
import ..DEFAULT_VALUE_TYPE
import ..Quantity
import ..Dimensions
import ..ustrip
import ..dimension
import ..AbstractDimensions
import .._pow
import ..constructor_of
import ..tryrationalize


@assert DEFAULT_VALUE_TYPE == Float64 "`units.jl` must be updated to support a different default value type."

const _UNIT_SYMBOLS = Symbol[]
const _UNIT_VALUES = Quantity{DEFAULT_VALUE_TYPE,DEFAULT_DIM_TYPE}[]

macro register_unit(name, value)
    return esc(_register_unit(name, value))
end

macro add_prefixes(base_unit, prefixes)
    @assert prefixes.head == :tuple
    return esc(_add_prefixes(base_unit, prefixes.args))
end

function _register_unit(name::Symbol, value)
    s = string(name)
    return quote
        const $name = $value
        push!(_UNIT_SYMBOLS, Symbol($s))
        push!(_UNIT_VALUES, $name)
    end
end

function _add_prefixes(base_unit::Symbol, prefixes)
    all_prefixes = (
        f=1e-15, p=1e-12, n=1e-9, μ=1e-6, u=1e-6, m=1e-3, c=1e-2, d=1e-1,
        k=1e3, M=1e6, G=1e9
    )
    expr = Expr(:block)
    for (prefix, value) in zip(keys(all_prefixes), values(all_prefixes))
        prefix in prefixes || continue
        new_unit = Symbol(prefix, base_unit)
        push!(expr.args, _register_unit(new_unit, :($value * $base_unit)))
    end
    return expr
end

# SI base units
@register_unit m Quantity(1.0, length=1)
@register_unit g Quantity(1e-3, mass=1)
@register_unit s Quantity(1.0, time=1)
@register_unit A Quantity(1.0, current=1)
@register_unit K Quantity(1.0, temperature=1)
@register_unit cd Quantity(1.0, luminosity=1)
@register_unit mol Quantity(1.0, amount=1)

@add_prefixes m (f, p, n, μ, u, c, d, m, k, M, G)
@add_prefixes g (μ, u, m, k)
@add_prefixes s (f, p, n, μ, u, m)
@add_prefixes A (n, μ, u, m, k)
@add_prefixes K (m,)
@add_prefixes cd (m,)
@add_prefixes mol (m,)

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

# Common assorted units
## Time
@register_unit min 60 * s
@register_unit h 60 * min
@register_unit hr h
@register_unit day 24 * h
@register_unit yr 365.25 * day

@add_prefixes min ()
@add_prefixes h ()
@add_prefixes hr ()
@add_prefixes day ()
@add_prefixes yr (k, M, G)

## Volume
@register_unit L dm^3

@add_prefixes L (m, d)

## Pressure
@register_unit bar 100 * kPa

@add_prefixes bar ()

# Do not wish to define Gaussian units, as it changes
# some formulas. Safer to force user to work exclusively in one unit system.

# Do not wish to define physical constants, as the number of symbols might lead to ambiguity.
# The user should define these instead.

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

"""A tuple of all possible unit symbols."""
const UNIT_SYMBOLS = Tuple(_UNIT_SYMBOLS)
const UNIT_VALUES = Tuple(_UNIT_VALUES)
const UNIT_MAPPING = NamedTuple([s => i for (i, s) in enumerate(UNIT_SYMBOLS)])


"""
    UnitDimensions{R} <: AbstractDimensions{R}

An `AbstractDimensions` with one dimension for every unit symbol.
This is to allow for lazily reducing to SI base units, whereas
`Dimensions` is always in SI base units. Furthermore, `UnitDimensions`
stores dimensions using a sparse vector for efficiency (since there
are so many unit symbols).
"""
struct UnitDimensions{R} <: AbstractDimensions{R}
    _data::SA.SparseVector{R}

    UnitDimensions(data::SA.SparseVector) = new{eltype(data)}(data)
    UnitDimensions{_R}(data::SA.SparseVector) where {_R} = new{_R}(data)
end

data(d::UnitDimensions) = getfield(d, :_data)
constructor_of(::Type{<:UnitDimensions}) = UnitDimensions

UnitDimensions{R}(d::UnitDimensions) where {R} = UnitDimensions{R}(data(d))
(::Type{D})(::Type{R}; kws...) where {R,D<:UnitDimensions} =
    let constructor=constructor_of(D){R}
        length(kws) == 0 && return constructor(SA.spzeros(R, length(UNIT_SYMBOLS)))
        I = [UNIT_MAPPING[s] for s in keys(kws)]
        V = [tryrationalize(R, v) for v in values(kws)]
        data = SA.sparsevec(I, V, length(UNIT_SYMBOLS))
        return constructor(data)
    end

function Base.convert(::Type{Q}, q::Quantity{<:Any,<:UnitDimensions}) where {T,D,Q<:Quantity{T,D}}
    result = one(Q) * ustrip(q)
    d = dimension(q)
    for (idx, value) in zip(SA.findnz(data(d))...)
        result = result * UNIT_VALUES[idx] ^ value
    end
    return result
end

static_fieldnames(::Type{<:UnitDimensions}) = UNIT_SYMBOLS
Base.getproperty(d::UnitDimensions{R}, s::Symbol) where {R} = data(d)[UNIT_MAPPING[s]]
Base.getindex(d::UnitDimensions{R}, k::Symbol) where {R} = getproperty(d, k)
Base.copy(d::UnitDimensions) = UnitDimensions(copy(data(d)))
Base.:(==)(l::UnitDimensions, r::UnitDimensions) = data(l) == data(r)
Base.iszero(d::UnitDimensions) = iszero(data(d))
Base.:*(l::UnitDimensions, r::UnitDimensions) = UnitDimensions(data(l) + data(r))
Base.:/(l::UnitDimensions, r::UnitDimensions) = UnitDimensions(data(l) - data(r))
Base.inv(d::UnitDimensions) = UnitDimensions(-data(d))
_pow(l::UnitDimensions{R}, r::R) where {R} = UnitDimensions(data(l) * r)


"""
    UnitSymbols

A separate module where each unit is treated as a separate dimension,
to enable pretty-printing of units.
"""
module UnitSymbols

    import ..UNIT_SYMBOLS
    import ..UnitDimensions

    import ...Quantity
    import ...DEFAULT_VALUE_TYPE
    import ...DEFAULT_DIM_BASE_TYPE

    # Lazily create unit symbols (since there are so many)
    const UNIT_SYMBOLS_EXIST = Ref{Bool}(false)
    const UNIT_SYMBOLS_LOCK = Threads.SpinLock()
    function _generate_unit_symbols()
        UNIT_SYMBOLS_EXIST[] || lock(UNIT_SYMBOLS_LOCK) do
            UNIT_SYMBOLS_EXIST[] && return nothing
            for unit in UNIT_SYMBOLS
                @eval const $unit = Quantity(1.0, UnitDimensions{DEFAULT_DIM_BASE_TYPE}; $(unit)=1)
            end
            UNIT_SYMBOLS_EXIST[] = true
        end
        return nothing
    end

    function uparse(raw_string::AbstractString)
        _generate_unit_symbols()
        raw_result = eval(Meta.parse(raw_string))
        return copy(as_quantity(raw_result))::Quantity{DEFAULT_VALUE_TYPE,UnitDimensions{DEFAULT_DIM_BASE_TYPE}}
    end

    as_quantity(q::Quantity) = q
    as_quantity(x::Number) = Quantity(convert(DEFAULT_VALUE_TYPE, x), UnitDimensions{DEFAULT_DIM_BASE_TYPE})
    as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")
end

end
