import .Units: UNIT_SYMBOLS, UNIT_MAPPING, UNIT_VALUES
import .Constants: CONSTANT_SYMBOLS, CONSTANT_MAPPING, CONSTANT_VALUES


const SYMBOL_CONFLICTS = intersect(UNIT_SYMBOLS, CONSTANT_SYMBOLS)

disambiguate_symbol(s) = s in SYMBOL_CONFLICTS ? Symbol(s, :_constant) : s

const INDEX_TYPE = UInt8
# Prefer units over constants:
# For example, this means we can't have a symbolic Planck's constant,
# as it is just "hours" (h), which is more common.
const ALL_SYMBOLS = (
    UNIT_SYMBOLS...,
    disambiguate_symbol.(CONSTANT_SYMBOLS)...
)
const ALL_VALUES = (UNIT_VALUES..., CONSTANT_VALUES...)
const ALL_MAPPING = NamedTuple{ALL_SYMBOLS}(INDEX_TYPE(1):INDEX_TYPE(length(ALL_SYMBOLS)))

"""
    AbstractSymbolicDimensions{R} <: AbstractDimensions{R}

Abstract type to allow for custom types of symbolic dimensions.
In defining this abstract type we allow for units to declare themselves
as a special type of symbolic dimensions which are immutable, whereas
the regular `SymbolicDimensions` type has mutable storage.
"""
abstract type AbstractSymbolicDimensions{R} <: AbstractDimensions{R} end

"""
    SymbolicDimensions{R} <: AbstractDimensions{R}

An `AbstractDimensions` with one dimension for every unit and constant symbol.
This is to allow for lazily reducing to SI base units, whereas
`Dimensions` is always in SI base units. Furthermore, `SymbolicDimensions`
stores dimensions using a sparse vector for efficiency (since there
are so many unit symbols).

You can convert a quantity using `SymbolicDimensions` as its dimensions
to one which uses `Dimensions` as its dimensions (i.e., base SI units)
`uexpand`.
"""
struct SymbolicDimensions{R} <: AbstractSymbolicDimensions{R}
    nzdims::Vector{INDEX_TYPE}
    nzvals::Vector{R}
end

"""
    SymbolicDimensionsSingleton{R} <: AbstractSymbolicDimensions{R}

This special symbolic dimensions types stores a single unit or constant, and can
be used for constructing symbolic units and constants without needing to allocate mutable storage.
"""
struct SymbolicDimensionsSingleton{R} <: AbstractSymbolicDimensions{R}
    dim::INDEX_TYPE

    SymbolicDimensionsSingleton(dim::INDEX_TYPE, ::Type{_R}) where {_R} = new{_R}(dim)
end

# Access:
function Base.getproperty(d::SymbolicDimensions{R}, s::Symbol) where {R}
    nzdims = DynamicQuantities.nzdims(d)
    i = get(ALL_MAPPING, s, INDEX_TYPE(0))
    iszero(i) && error("$s is not available as a symbol in `SymbolicDimensions`. Symbols available: $(ALL_SYMBOLS).")
    ii = searchsortedfirst(nzdims, i)
    if ii <= length(nzdims) && nzdims[ii] == i
        return nzvals(d)[ii]
    else
        return zero(R)
    end
end
function Base.getproperty(d::SymbolicDimensionsSingleton{R}, s::Symbol) where {R}
    i = get(ALL_MAPPING, s, INDEX_TYPE(0))
    iszero(i) && error("$s is not available as a symbol in `SymbolicDimensionsSingleton`. Symbols available: $(ALL_SYMBOLS).")
    return i == getfield(d, :dim) ? one(R) : zero(R)
end


# Constructors:
(::Type{<:SymbolicDimensions})(::Type{R}; kws...) where {R} = SymbolicDimensions{R}(; kws...)
SymbolicDimensions{R}(d::SymbolicDimensions) where {R} = SymbolicDimensions{R}(nzdims(d), convert(Vector{R}, nzvals(d)))
SymbolicDimensions(; kws...) = SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}(; kws...)
SymbolicDimensionsSingleton(s::Symbol) = SymbolicDimensionsSingleton{DEFAULT_DIM_BASE_TYPE}(s)
function SymbolicDimensions{R}(; kws...) where {R}
    if isempty(kws)
        return SymbolicDimensions{R}(Vector{INDEX_TYPE}(undef, 0), Vector{R}(undef, 0))
    end
    I = INDEX_TYPE[ALL_MAPPING[s] for s in keys(kws)]
    p = sortperm(I)
    V = R[tryrationalize(R, kws[i]) for i in p]
    return SymbolicDimensions{R}(permute!(I, p), V)
end
function SymbolicDimensionsSingleton{R}(s::Symbol) where {R}
    i = get(ALL_MAPPING, s, INDEX_TYPE(0))
    iszero(i) && error("$s is not available as a symbol in `SymbolicDimensionsSingleton`. Symbols available: $(ALL_SYMBOLS).")
    return SymbolicDimensionsSingleton(i, R)
end

# Traits:
dimension_names(::Type{<:AbstractSymbolicDimensions}) = ALL_SYMBOLS
Base.propertynames(::AbstractSymbolicDimensions) = ALL_SYMBOLS
Base.getindex(d::AbstractSymbolicDimensions, k::Symbol) = getproperty(d, k)
constructorof(::Type{<:SymbolicDimensions}) = SymbolicDimensions
constructorof(::Type{<:SymbolicDimensionsSingleton{R}}) where {R} = SymbolicDimensionsSingleton{R}
with_type_parameters(::Type{<:SymbolicDimensions}, ::Type{R}) where {R} = SymbolicDimensions{R}
with_type_parameters(::Type{<:SymbolicDimensionsSingleton}, ::Type{R}) where {R} = SymbolicDimensionsSingleton{R}
nzdims(d::SymbolicDimensions) = getfield(d, :nzdims)
nzdims(d::SymbolicDimensionsSingleton) = (getfield(d, :dim),)
nzvals(d::SymbolicDimensions) = getfield(d, :nzvals)
nzvals(::SymbolicDimensionsSingleton{R}) where {R} = (one(R),)
Base.eltype(::AbstractSymbolicDimensions{R}) where {R} = R
Base.eltype(::Type{<:AbstractSymbolicDimensions{R}}) where {R} = R

# Conversion:
function SymbolicDimensions(d::SymbolicDimensionsSingleton{R}) where {R}
    return SymbolicDimensions{R}([nzdims(d)...], [nzvals(d)...])
end
function SymbolicDimensions{R}(d::SymbolicDimensionsSingleton) where {R}
    return SymbolicDimensions{R}([nzdims(d)...], [nzvals(d)...])
end
Base.convert(::Type{SymbolicDimensions}, d::SymbolicDimensionsSingleton) = SymbolicDimensions(d)
Base.convert(::Type{SymbolicDimensions{R}}, d::SymbolicDimensionsSingleton) where {R} = SymbolicDimensions{R}(d)

for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,Q<:$type{T,SymbolicDimensions}}
            return convert(with_type_parameters(Q, T,SymbolicDimensions{DEFAULT_DIM_BASE_TYPE}), q)
        end
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,R,Q<:$type{T,SymbolicDimensions{R}}}
            syms = (:m, :kg, :s, :A, :K, :cd, :mol)
            vals = (ulength(q), umass(q), utime(q), ucurrent(q), utemperature(q), uluminosity(q), uamount(q))
            I = INDEX_TYPE[ALL_MAPPING[s] for (s, v) in zip(syms, vals) if !iszero(v)]
            V = R[tryrationalize(R, v) for v in vals if !iszero(v)]
            p = sortperm(I)
            permute!(I, p)
            permute!(V, p)
            dims = SymbolicDimensions{R}(I, V)
            return constructorof(Q)(convert(T, ustrip(q)), dims)
        end
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:AbstractSymbolicDimensions}) where {T,D<:Dimensions,Q<:$type{T,D}}
            result = constructorof(Q)(convert(T, ustrip(q)), D())
            d = dimension(q)
            for (idx, value) in zip(nzdims(d), nzvals(d))
                if !iszero(value)
                    result *= convert(with_type_parameters(Q, T, D), ALL_VALUES[idx] ^ value)
                end
            end
            return result
        end
    end
end


"""
    uexpand(q::UnionAbstractQuantity{<:Any,<:AbstractSymbolicDimensions})

Expand the symbolic units in a quantity to their base SI form.
In other words, this converts a quantity with `AbstractSymbolicDimensions`
to one with `Dimensions`. The opposite of this function is `uconvert`,
for converting to specific symbolic units, or, e.g., `convert(Quantity{<:Any,<:AbstractSymbolicDimensions}, q)`,
for assuming SI units as the output symbols.
"""
function uexpand(q::Q) where {T,R,D<:AbstractSymbolicDimensions{R},Q<:UnionAbstractQuantity{T,D}}
    return convert(with_type_parameters(Q, T, Dimensions{R}), q)
end
uexpand(q::QuantityArray) = uexpand.(q)
# TODO: Make the array-based one more efficient

"""
    uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractSymbolicDimensions}, q::UnionAbstractQuantity{<:Any, <:Dimensions})

Convert a quantity `q` with base SI units to the symbolic units of `qout`, for `q` and `qout` with compatible units.
Mathematically, the result has value `q / uexpand(qout)` and units `dimension(qout)`. 
"""
function uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractSymbolicDimensions}, q::UnionAbstractQuantity{<:Any, <:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    qout_expanded = uexpand(qout)
    dimension(q) == dimension(qout_expanded) || throw(DimensionError(q, qout_expanded))
    new_val = ustrip(q) / ustrip(qout_expanded)
    new_dim = dimension(qout)
    return new_quantity(typeof(q), new_val, new_dim)
end
function uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractSymbolicDimensions}, q::QuantityArray{<:Any,<:Any,<:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    qout_expanded = uexpand(qout)
    dimension(q) == dimension(qout_expanded) || throw(DimensionError(q, qout_expanded))
    new_array = ustrip(q) ./ ustrip(qout_expanded)
    new_dim = dimension(qout)
    return QuantityArray(new_array, new_dim, quantity_type(q))
end
# TODO: Method for converting SymbolicDimensions -> SymbolicDimensions

"""
    uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractSymbolicDimensions})

Create a function that converts an input quantity `q` with base SI units to the symbolic units of `qout`, i.e 
a function equivalent to `q -> uconvert(qout, q)`.
"""
uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractSymbolicDimensions}) = Base.Fix1(uconvert, qout)

Base.copy(d::SymbolicDimensions) = SymbolicDimensions(copy(nzdims(d)), copy(nzvals(d)))
Base.copy(d::SymbolicDimensionsSingleton) = SymbolicDimensionsSingleton(getfield(d, :dim), eltype(d))

function Base.:(==)(l::AbstractSymbolicDimensions, r::AbstractSymbolicDimensions)
    nzdims_l = nzdims(l)
    nzvals_l = nzvals(l)
    nzdims_r = nzdims(r)
    nzvals_r = nzvals(r)
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
Base.iszero(d::AbstractSymbolicDimensions) = iszero(nzvals(d))
Base.iszero(d::SymbolicDimensionsSingleton) = false

# Defines `inv(::SymbolicDimensions)` and `^(::SymbolicDimensions, ::Number)`
function map_dimensions(op::Function, d::SymbolicDimensions)
    return SymbolicDimensions(copy(nzdims(d)), map(op, nzvals(d)))
end
# Ensure we always do operations with SymbolicDimensions:
map_dimensions(op::Function, d::SymbolicDimensionsSingleton) = map_dimensions(op, SymbolicDimensions(d))

# Defines `*(::SymbolicDimensions, ::SymbolicDimensions)` and `/(::SymbolicDimensions, ::SymbolicDimensions)`
function map_dimensions(op::O, l::SymbolicDimensions{L}, r::SymbolicDimensions{R}) where {O<:Function,L,R}
    zero_L = zero(L)
    zero_R = zero(R)
    T = typeof(op(zero(L), zero(R)))
    I = Vector{INDEX_TYPE}(undef, 0)
    V = Vector{T}(undef, 0)
    nzdims_l = nzdims(l)
    nzvals_l = nzvals(l)
    nzdims_r = nzdims(r)
    nzvals_r = nzvals(r)
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
# Ensure we always do operations with SymbolicDimensions:
map_dimensions(op::Function, l::SymbolicDimensionsSingleton, r::SymbolicDimensionsSingleton) = map_dimensions(op, SymbolicDimensions(l), SymbolicDimensions(r))
map_dimensions(op::Function, l::SymbolicDimensions, r::SymbolicDimensionsSingleton) = map_dimensions(op, l, SymbolicDimensions(r))
map_dimensions(op::Function, l::SymbolicDimensionsSingleton, r::SymbolicDimensions) = map_dimensions(op, SymbolicDimensions(l), r)

# Units are stored using SymbolicDimensionsSingleton
const DEFAULT_SYMBOLIC_QUANTITY_TYPE = with_type_parameters(DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE, SymbolicDimensionsSingleton{DEFAULT_DIM_BASE_TYPE})
# However, we output units from `us_str` using SymbolicDimensions, for type stability
const DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE = with_type_parameters(DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE, SymbolicDimensions{DEFAULT_DIM_BASE_TYPE})

"""
    SymbolicUnits

A separate module where each unit is treated as a separate dimension,
to enable pretty-printing of units.
"""
module SymbolicUnits

    import ..UNIT_SYMBOLS
    import ..SymbolicDimensionsSingleton
    import ...constructorof
    import ...DEFAULT_SYMBOLIC_QUANTITY_TYPE
    import ...DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE
    import ...DEFAULT_VALUE_TYPE
    import ...DEFAULT_DIM_BASE_TYPE

    # Lazily create unit symbols (since there are so many)
    module Constants
        import ...CONSTANT_SYMBOLS
        import ...SymbolicDimensionsSingleton
        import ...constructorof
        import ...disambiguate_symbol
        import ....DEFAULT_SYMBOLIC_QUANTITY_TYPE
        import ....DEFAULT_VALUE_TYPE
        import ....DEFAULT_DIM_BASE_TYPE

        for unit in CONSTANT_SYMBOLS
            @eval const $unit = constructorof(DEFAULT_SYMBOLIC_QUANTITY_TYPE)(
                DEFAULT_VALUE_TYPE(1.0),
                SymbolicDimensionsSingleton{DEFAULT_DIM_BASE_TYPE}($(QuoteNode(disambiguate_symbol(unit))))
            )
        end
    end
    import .Constants
    import .Constants as SymbolicConstants

    for unit in UNIT_SYMBOLS
        @eval const $unit = constructorof(DEFAULT_SYMBOLIC_QUANTITY_TYPE)(
            DEFAULT_VALUE_TYPE(1.0),
            SymbolicDimensionsSingleton{DEFAULT_DIM_BASE_TYPE}($(QuoteNode(unit)))
        )
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
        raw_result = eval(Meta.parse(raw_string))
        return copy(as_quantity(raw_result))::DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE
    end

    as_quantity(q::DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE) = q
    as_quantity(x::Number) = convert(DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE, x)
    as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")
end

import .SymbolicUnits: sym_uparse
import .SymbolicUnits: SymbolicConstants

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
    return esc(SymbolicUnits.sym_uparse(s))
end

function Base.promote_rule(::Type{SymbolicDimensionsSingleton{R1}}, ::Type{SymbolicDimensionsSingleton{R2}}) where {R1,R2}
    return SymbolicDimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{SymbolicDimensionsSingleton{R1}}, ::Type{D}) where {R1,D<:AbstractDimensions}
    return promote_type(SymbolicDimensions{R1}, D)
end
function Base.promote_rule(::Type{SymbolicDimensions{R1}}, ::Type{SymbolicDimensions{R2}}) where {R1,R2}
    return SymbolicDimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{SymbolicDimensions{R1}}, ::Type{Dimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{Dimensions{R2}}, ::Type{SymbolicDimensions{R1}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
