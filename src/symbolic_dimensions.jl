import .Units: UNIT_SYMBOLS, UNIT_MAPPING, UNIT_VALUES
import .Constants: CONSTANT_SYMBOLS, CONSTANT_MAPPING, CONSTANT_VALUES
import SparseArrays as SA

const SYMBOL_CONFLICTS = intersect(UNIT_SYMBOLS, CONSTANT_SYMBOLS)

# Prefer units over constants:
# For example, this means we can't have a symbolic Planck's constant,
# as it is just "hours" (h), which is more common.
const ALL_SYMBOLS = (
    UNIT_SYMBOLS...,
    setdiff(CONSTANT_SYMBOLS, SYMBOL_CONFLICTS)...
)
const ALL_VALUES = vcat(
    UNIT_VALUES...,
    (
        v
        for (k, v) in zip(CONSTANT_SYMBOLS, CONSTANT_VALUES)
        if k ∉ SYMBOL_CONFLICTS
    )...
)
const ALL_MAPPING = NamedTuple([s => i for (i, s) in enumerate(ALL_SYMBOLS)])

"""
    SymbolicDimensions{R} <: AbstractDimensions{R}

An `AbstractDimensions` with one dimension for every unit and constant symbol.
This is to allow for lazily reducing to SI base units, whereas
`Dimensions` is always in SI base units. Furthermore, `SymbolicDimensions`
stores dimensions using a sparse vector for efficiency (since there
are so many unit symbols).

You can convert a quantity using `SymbolicDimensions` as its dimensions
to one which uses `Dimensions` as its dimensions (i.e., base SI units)
`expand_units`.
"""
struct SymbolicDimensions{R} <: AbstractDimensions{R}
    _data::SA.SparseVector{R}

    SymbolicDimensions(data::SA.SparseVector) = new{eltype(data)}(data)
    SymbolicDimensions{_R}(data::SA.SparseVector) where {_R} = new{_R}(data)
end

static_fieldnames(::Type{<:SymbolicDimensions}) = ALL_SYMBOLS
data(d::SymbolicDimensions) = getfield(d, :_data)
Base.getproperty(d::SymbolicDimensions{R}, s::Symbol) where {R} = data(d)[ALL_MAPPING[s]]
Base.getindex(d::SymbolicDimensions{R}, k::Symbol) where {R} = getproperty(d, k)
constructor_of(::Type{<:SymbolicDimensions}) = SymbolicDimensions

SymbolicDimensions{R}(d::SymbolicDimensions) where {R} = SymbolicDimensions{R}(data(d))
(::Type{D})(; kws...) where {D<:SymbolicDimensions} = D(DEFAULT_DIM_BASE_TYPE; kws...)
(::Type{D})(::Type{R}; kws...) where {R,D<:SymbolicDimensions} =
    let constructor=constructor_of(D){R}
        length(kws) == 0 && return constructor(SA.spzeros(R, length(ALL_SYMBOLS)))
        I = [ALL_MAPPING[s] for s in keys(kws)]
        V = [tryrationalize(R, v) for v in values(kws)]
        data = SA.sparsevec(I, V, length(ALL_SYMBOLS))
        return constructor(data)
    end

function Base.convert(::Type{Qout}, q::Quantity{<:Any,<:Dimensions}) where {T,D<:SymbolicDimensions,Qout<:Quantity{T,D}}
    output = Qout(
        convert(T, ustrip(q)),
        D;
        m=ulength(q),
        kg=umass(q),
        s=utime(q),
        A=ucurrent(q),
        K=utemperature(q),
        cd=uluminosity(q),
        mol=uamount(q),
    )
    SA.dropzeros!(data(dimension(output)))
    return output
end
function Base.convert(::Type{Q}, q::Quantity{<:Any,<:SymbolicDimensions}) where {T,D<:Dimensions,Q<:Quantity{T,D}}
    result = one(Q) * ustrip(q)
    d = dimension(q)
    for (idx, value) in zip(SA.findnz(data(d))...)
        result = result * convert(Q, ALL_VALUES[idx]) ^ value
    end
    return result
end

"""
    expand_units(q::Quantity{<:Any,<:SymbolicDimensions})

Expand the symbolic units in a quantity to their base SI form.
In other words, this converts a `Quantity` with `SymbolicDimensions`
to one with `Dimensions`.
"""
function expand_units(q::Q) where {T,R,D<:SymbolicDimensions{R},Q<:AbstractQuantity{T,D}}
    return convert(constructor_of(Q){T,Dimensions{R}}, q)
end
expand_units(q::QuantityArray) = expand_units.(q)


Base.copy(d::SymbolicDimensions) = SymbolicDimensions(copy(data(d)))
Base.:(==)(l::SymbolicDimensions, r::SymbolicDimensions) = data(l) == data(r)
Base.iszero(d::SymbolicDimensions) = iszero(data(d))
Base.:*(l::SymbolicDimensions, r::SymbolicDimensions) = SymbolicDimensions(data(l) + data(r))
Base.:/(l::SymbolicDimensions, r::SymbolicDimensions) = SymbolicDimensions(data(l) - data(r))
Base.inv(d::SymbolicDimensions) = SymbolicDimensions(-data(d))
Base.:^(l::SymbolicDimensions{R}, r::Integer) where {R} = SymbolicDimensions(data(l) * r)
Base.:^(l::SymbolicDimensions{R}, r::Number) where {R} = SymbolicDimensions(data(l) * tryrationalize(R, r))


"""
    SymbolicUnitsParse

A separate module where each unit is treated as a separate dimension,
to enable pretty-printing of units.
"""
module SymbolicUnitsParse

    import ..UNIT_SYMBOLS
    import ..CONSTANT_SYMBOLS
    import ..SYMBOL_CONFLICTS
    import ..SymbolicDimensions

    import ...Quantity
    import ...DEFAULT_VALUE_TYPE
    import ...DEFAULT_DIM_BASE_TYPE

    # Lazily create unit symbols (since there are so many)
    module Constants
        import ..CONSTANT_SYMBOLS
        import ..SYMBOL_CONFLICTS
        import ..SymbolicDimensions

        import ..Quantity
        import ..DEFAULT_VALUE_TYPE
        import ..DEFAULT_DIM_BASE_TYPE

        import ...Constants as EagerConstants

        const CONSTANT_SYMBOLS_EXIST = Ref{Bool}(false)
        const CONSTANT_SYMBOLS_LOCK = Threads.SpinLock()
        function _generate_unit_symbols()
            CONSTANT_SYMBOLS_EXIST[] || lock(CONSTANT_SYMBOLS_LOCK) do
                CONSTANT_SYMBOLS_EXIST[] && return nothing
                for unit in setdiff(CONSTANT_SYMBOLS, SYMBOL_CONFLICTS)
                    @eval const $unit = Quantity(DEFAULT_VALUE_TYPE(1.0), SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}; $(unit)=1)
                end
                # Evaluate conflicting symbols to non-symbolic form:
                for unit in SYMBOL_CONFLICTS
                    @eval const $unit = convert(Quantity{DEFAULT_VALUE_TYPE,SymbolicDimensions}, EagerConstants.$unit)
                end
                CONSTANT_SYMBOLS_EXIST[] = true
            end
            return nothing
        end
    end
    import .Constants

    const UNIT_SYMBOLS_EXIST = Ref{Bool}(false)
    const UNIT_SYMBOLS_LOCK = Threads.SpinLock()
    function _generate_unit_symbols()
        UNIT_SYMBOLS_EXIST[] || lock(UNIT_SYMBOLS_LOCK) do
            UNIT_SYMBOLS_EXIST[] && return nothing
            for unit in UNIT_SYMBOLS
                @eval const $unit = Quantity(DEFAULT_VALUE_TYPE(1.0), SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}; $(unit)=1)
            end
            UNIT_SYMBOLS_EXIST[] = true
        end
        return nothing
    end

    """
        sym_uparse(raw_string::AbstractString)

    Parse a string containing an expression of units and return the
    corresponding `Quantity` object with `Float64` value.
    However, that unlike the regular `u"..."` macro, this macro uses
    `SymbolicDimensions` for the dimension type, which means that all units and
    constants are stored symbolically and will not automatically expand to SI
    units. For example, `sym_uparse("km/s^2")` would be parsed to
    `Quantity(1.0, SymbolicDimensions, km=1, s=-2)`.

    Note that inside this expression, you also have access to the `Constants`
    module. So, for example, `sym_uparse("Constants.c^2 * Hz^2")` would evaluate to
    `Quantity(1.0, SymbolicDimensions, c=2, Hz=2)`. However, note that due to
    namespace collisions, a few physical constants are automatically converted.
    """
    function sym_uparse(raw_string::AbstractString)
        _generate_unit_symbols()
        Constants._generate_unit_symbols()
        raw_result = eval(Meta.parse(raw_string))
        return copy(as_quantity(raw_result))::Quantity{DEFAULT_VALUE_TYPE,SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}}
    end

    as_quantity(q::Quantity) = q
    as_quantity(x::Number) = Quantity(convert(DEFAULT_VALUE_TYPE, x), SymbolicDimensions{DEFAULT_DIM_BASE_TYPE})
    as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")
end

import .SymbolicUnitsParse: sym_uparse

"""
    us"[unit expression]"

Parse a string containing an expression of units and return the
corresponding `Quantity` object with `Float64` value. However,
unlike the regular `u"..."` macro, this macro uses `SymbolicDimensions`
for the dimension type, which means that all units and constants
are stored symbolically and will not automatically expand to SI units.
For example, `us"km/s^2"` would be parsed to `Quantity(1.0, SymbolicDimensions, km=1, s=-2)`.

Note that inside this expression, you also have access to the `Constants`
module. So, for example, `us"Constants.c^2 * Hz^2"` would evaluate to
`Quantity(1.0, SymbolicDimensions, c=2, Hz=2)`. However, note that due to
namespace collisions, a few physical constants are automatically converted.
"""
macro us_str(s)
    return esc(SymbolicUnitsParse.sym_uparse(s))
end
