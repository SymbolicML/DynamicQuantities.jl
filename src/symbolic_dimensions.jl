import .Units: UNIT_SYMBOLS, UNIT_MAPPING, UNIT_VALUES
import .Constants: CONSTANT_SYMBOLS, CONSTANT_MAPPING, CONSTANT_VALUES

const SYMBOL_CONFLICTS = intersect(UNIT_SYMBOLS, CONSTANT_SYMBOLS)

const INDEX_TYPE = UInt8
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
const ALL_MAPPING = NamedTuple([s => INDEX_TYPE(i) for (i, s) in enumerate(ALL_SYMBOLS)])

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
    nzdims::Vector{INDEX_TYPE}
    nzvals::Vector{R}
end

static_fieldnames(::Type{<:SymbolicDimensions}) = ALL_SYMBOLS
function Base.getproperty(d::SymbolicDimensions{R}, s::Symbol) where {R}
    nzdims = getfield(d, :nzdims)
    i = get(ALL_MAPPING, s, INDEX_TYPE(0))
    iszero(i) && error("$s is not available as a symbol in SymbolicDimensions. Symbols available: $(ALL_SYMBOLS).")
    ii = searchsortedfirst(nzdims, i)
    if ii <= length(nzdims) && nzdims[ii] == i
        return getfield(d, :nzvals)[ii]
    else
        return zero(R)
    end
end
Base.propertynames(::SymbolicDimensions) = ALL_SYMBOLS
Base.getindex(d::SymbolicDimensions, k::Symbol) = getproperty(d, k)
constructor_of(::Type{<:SymbolicDimensions}) = SymbolicDimensions

SymbolicDimensions{R}(d::SymbolicDimensions) where {R} = SymbolicDimensions{R}(getfield(d, :nzdims), convert(Vector{R}, getfield(d, :nzvals)))
SymbolicDimensions(; kws...) = SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}(; kws...)
function SymbolicDimensions{R}(; kws...) where {R}
    if isempty(kws)
        return SymbolicDimensions{R}(Vector{INDEX_TYPE}(undef, 0), Vector{R}(undef, 0))
    end
    I = INDEX_TYPE[ALL_MAPPING[s] for s in keys(kws)]
    p = sortperm(I)
    V = R[tryrationalize(R, kws[i]) for i in p]
    return SymbolicDimensions{R}(permute!(I, p), V)
end
(::Type{<:SymbolicDimensions})(::Type{R}; kws...) where {R} = SymbolicDimensions{R}(; kws...)

function Base.convert(::Type{Quantity{T,SymbolicDimensions}}, q::Quantity{<:Any,<:Dimensions}) where {T}
    return convert(Quantity{T,SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}}, q)
end
function Base.convert(::Type{Quantity{T,SymbolicDimensions{R}}}, q::Quantity{<:Any,<:Dimensions}) where {T,R}
    syms = (:m, :kg, :s, :A, :K, :cd, :mol)
    vals = (ulength(q), umass(q), utime(q), ucurrent(q), utemperature(q), uluminosity(q), uamount(q))
    I = INDEX_TYPE[ALL_MAPPING[s] for (s, v) in zip(syms, vals) if !iszero(v)]
    V = R[tryrationalize(R, v) for v in vals if !iszero(v)]
    p = sortperm(I)
    permute!(I, p)
    permute!(V, p)
    dims = SymbolicDimensions{R}(I, V)
    return Quantity(convert(T, ustrip(q)), dims)
end
function Base.convert(::Type{Quantity{T,D}}, q::Quantity{<:Any,<:SymbolicDimensions}) where {T,D<:Dimensions}
    result = Quantity(T(ustrip(q)), D())
    d = dimension(q)
    for (idx, value) in zip(getfield(d, :nzdims), getfield(d, :nzvals))
        if !iszero(value)
            result = result * convert(Quantity{T,D}, ALL_VALUES[idx]) ^ value
        end
    end
    return result
end

"""
    expand_units(q::Quantity{<:Any,<:SymbolicDimensions})

Expand the symbolic units in a quantity to their base SI form.
In other words, this converts a `Quantity` with `SymbolicDimensions`
to one with `Dimensions`. The opposite of this function is `uconvert`,
for converting to specific symbolic units, or `convert(Quantity{<:Any,<:SymbolicDimensions}, q)`,
for assuming SI units as the output symbols.
"""
function expand_units(q::Q) where {T,R,D<:SymbolicDimensions{R},Q<:AbstractQuantity{T,D}}
    return convert(constructor_of(Q){T,Dimensions{R}}, q)
end
expand_units(q::QuantityArray) = expand_units.(q)
# TODO: Make the array-based one more efficient

"""
    uconvert(qout::AbstractQuantity{<:Any, <:SymbolicDimensions}, q::AbstractQuantity{<:Any, <:Dimensions})

Convert a quantity `q` with base SI units to the symbolic units of `qout`, for `q` and `qout` with compatible units.
Mathematically, the result has value `q / expand_units(qout)` and units `dimension(qout)`. 
"""
function uconvert(qout::AbstractQuantity{<:Any, <:SymbolicDimensions}, q::AbstractQuantity{<:Any, <:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    qout_expanded = expand_units(qout)
    dimension(q) == dimension(qout_expanded) || throw(DimensionError(q, qout_expanded))
    new_val = ustrip(q) / ustrip(qout_expanded)
    new_dim = dimension(qout)
    return new_quantity(typeof(q), new_val, new_dim)
end
function uconvert(qout::AbstractQuantity{<:Any,<:SymbolicDimensions}, q::QuantityArray{<:Any,<:Any,<:Dimensions})
    qout_expanded = expand_units(qout)
    dimension(q) == dimension(qout_expanded) || throw(DimensionError(q, qout_expanded))
    new_array = ustrip(q) ./ ustrip(qout_expanded)
    new_dim = dimension(qout)
    return QuantityArray(new_array, new_dim, quantity_type(q))
end
# TODO: Method for converting SymbolicDimensions -> SymbolicDimensions

"""
    uconvert(qout::AbstractQuantity{<:Any, <:SymbolicDimensions})

Create a function that converts an input quantity `q` with base SI units to the symbolic units of `qout`, i.e 
a function equivalent to `q -> uconvert(qout, q)`.
"""
uconvert(qout::AbstractQuantity{<:Any, <:SymbolicDimensions}) = Base.Fix1(uconvert, qout)

Base.copy(d::SymbolicDimensions) = SymbolicDimensions(copy(getfield(d, :nzdims)), copy(getfield(d, :nzvals)))
function Base.:(==)(l::SymbolicDimensions, r::SymbolicDimensions)
    nzdims_l = getfield(l, :nzdims)
    nzvals_l = getfield(l, :nzvals)
    nzdims_r = getfield(r, :nzdims)
    nzvals_r = getfield(r, :nzvals)
    nl = length(nzdims_l)
    nr = length(nzdims_r)
    il = ir = 1
    while il <= nl && ir <= nr
        dim_l = nzdims_l[il]
        dim_r = nzdims_r[ir]
        if dim_l == dim_r
            if nzvals_l[il] != nzvals_r[ir]
                return false
            end
            il += 1
            ir += 1
        elseif dim_l < dim_r
            if !iszero(nzvals_l[il])
                return false
            end
            il += 1
        else
            if !iszero(nzvals_r[ir])
                return false
            end
            ir += 1
        end
    end

    while il <= nl
        if !iszero(nzvals_l[il])
            return false
        end
        il += 1
    end

    while ir <= nr
        if !iszero(nzvals_r[ir])
            return false
        end
        ir += 1
    end

    return true
end
Base.iszero(d::SymbolicDimensions) = iszero(getfield(d, :nzvals))

# Defines `inv(::SymbolicDimensions)` and `^(::SymbolicDimensions, ::Number)`
function map_dimensions(op::Function, d::SymbolicDimensions)
    return SymbolicDimensions(copy(getfield(d, :nzdims)), map(op, getfield(d, :nzvals)))
end

# Defines `*(::SymbolicDimensions, ::SymbolicDimensions)` and `/(::SymbolicDimensions, ::SymbolicDimensions)`
function map_dimensions(op::O, l::SymbolicDimensions{L}, r::SymbolicDimensions{R}) where {O<:Function,L,R}
    zero_L = zero(L)
    zero_R = zero(R)
    T = typeof(op(zero(L), zero(R)))
    I = Vector{INDEX_TYPE}(undef, 0)
    V = Vector{T}(undef, 0)
    nzdims_l = getfield(l, :nzdims)
    nzvals_l = getfield(l, :nzvals)
    nzdims_r = getfield(r, :nzdims)
    nzvals_r = getfield(r, :nzvals)
    nl = length(nzdims_l)
    nr = length(nzdims_r)
    il = ir = 1
    while il <= nl && ir <= nr
        dim_l = nzdims_l[il]
        dim_r = nzdims_r[ir]
        if dim_l == dim_r
            s = op(nzvals_l[il], nzvals_r[ir])
            if !iszero(s)
                push!(I, dim_l)
                push!(V, s)
            end
            il += 1
            ir += 1
        elseif dim_l < dim_r
            s = op(nzvals_l[il], zero_R)
            if !iszero(s)
                push!(I, dim_l)
                push!(V, s)
            end
            il += 1
        else
            s = op(zero_L, nzvals_r[ir])
            if !iszero(s)
                push!(I, dim_r)
                push!(V, s)
            end
            ir += 1
        end
    end

    while il <= nl
        s = op(nzvals_l[il], zero_R)
        if !iszero(s)
            push!(I, nzdims_l[il])
            push!(V, s)
        end
        il += 1
    end

    while ir <= nr
        s = op(zero_L, nzvals_r[ir])
        if !iszero(s)
            push!(I, nzdims_r[ir])
            push!(V, s)
        end
        ir += 1
    end

    return SymbolicDimensions(I, V)
end

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
