const AbstractQuantityOrArray{T,D} = Union{UnionAbstractQuantity{T,D}, QuantityArray{T,<:Any,D}}

abstract type AbstractAffineDimensions{R} <: AbstractDimensions{R} end

const AffineOrSymbolicDimensions{R} = Union{AbstractAffineDimensions{R}, AbstractSymbolicDimensions{R}}

"""
    AffineOffsetError{D} <: Exception

Error thrown when attempting an implicit conversion of an `AffineDimensions` 
with a non-zero offset.

!!! warning
    This is an experimental feature and may change in the future.
"""
struct AffineOffsetError{D} <: Exception
    dim::D

    AffineOffsetError(dim) = new{typeof(dim)}(dim)
end

Base.showerror(io::IO, e::AffineOffsetError) = print(io, "AffineOffsetError: ", e.dim, " has a non-zero offset, implicit conversion is not allowed due to ambiguity. Use `uexpand(x)` to explicitly convert")

"""
    AffineDimensions{R}(scale::Float64, offset::Float64, basedim::Dimensions{R}, symbol::Symbol=:nothing)

AffineDimensions adds a scale and offset to Dimensions{R} allowing the expression of affine transformations of units (for example °C)
The offset parameter is in SI units (i.e. having the dimension of basedim)

!!! warning
    This is an experimental feature and may change in the future.
"""
struct AffineDimensions{R} <: AbstractAffineDimensions{R}
    scale::Float64
    offset::Float64
    basedim::Dimensions{R}
    symbol::Symbol
end

AffineDimensions(; scale=1.0, offset=0.0, basedim, symbol=:nothing) = AffineDimensions(scale, offset, basedim, symbol)
AffineDimensions{R}(; scale=1.0, offset=0.0, basedim, symbol=:nothing) where {R} = AffineDimensions{R}(scale, offset, basedim, symbol)
AffineDimensions(s, o, dims::AbstractDimensions{R}, symbol=:nothing) where {R} = AffineDimensions{R}(s, o, dims, symbol)
AffineDimensions(s, o, q::UnionAbstractQuantity{<:Any,<:AbstractDimensions{R}}, sym=:nothing) where {R} = AffineDimensions{R}(s, o, q, sym)
AffineDimensions(d::Dimensions{R}) where R = AffineDimensions{R}(scale=1.0, offset=0.0, basedim=d, symbol=:nothing)

# Handle offsets in affine dimensions
function AffineDimensions{R}(s::Real, o::Real, dims::AbstractAffineDimensions, sym=:nothing) where {R}
    new_s = s * affine_scale(dims)
    new_o = affine_offset(dims) + o * affine_scale(dims)
    return AffineDimensions{R}(new_s, new_o, affine_base_dim(dims), sym)
end

function AffineDimensions{R}(s::Real, o::UnionAbstractQuantity, dims::AbstractAffineDimensions, sym=:nothing) where {R}
    new_s = s * affine_scale(dims)
    new_o = affine_offset(dims) + ustrip(uexpand(o))
    return AffineDimensions{R}(new_s, new_o, affine_base_dim(dims), sym)
end

function AffineDimensions{R}(s::Real, o::UnionAbstractQuantity, dims::Dimensions, sym=:nothing) where {R}
    return AffineDimensions{R}(s, ustrip(uexpand(o)), dims, sym)
end

# From two quantities 
function AffineDimensions{R}(s::Real, o::UnionAbstractQuantity, q::UnionAbstractQuantity, sym=:nothing) where {R}
    q_si_origin = uexpand(0 * q)
    o_si_origin = uexpand(0 * o)
    o_difference_to_si = uexpand(o) - o_si_origin
    dimension(q_si_origin) == dimension(o_difference_to_si) || throw(DimensionError(o, q))
    o_si = o_difference_to_si + q_si_origin
    q_si = uexpand(q) - q_si_origin
    return AffineDimensions{R}(s, o_si, q_si, sym)
end

# Base case with SI units
function AffineDimensions{R}(s::Real, o::UnionAbstractQuantity{<:Any,<:Dimensions}, q::UnionAbstractQuantity{<:Any,<:Dimensions}, sym=:nothing) where {R}
    dimension(o) == dimension(q) || throw(DimensionError(o, q))
    return AffineDimensions{R}(s * ustrip(q), ustrip(o), dimension(q), sym)
end

# Offset from real
function AffineDimensions{R}(s::Real, o::Real, q::Q, sym=:nothing) where {R, Q<:UnionAbstractQuantity}
    return AffineDimensions{R}(s, o * q, q, sym)
end

affine_scale(d::AffineDimensions) = d.scale
affine_offset(d::AffineDimensions) = d.offset
affine_base_dim(d::AffineDimensions) = d.basedim

with_type_parameters(::Type{<:AffineDimensions}, ::Type{R}) where {R} = AffineDimensions{R}
@unstable constructorof(::Type{<:AffineDimensions}) = AffineDimensions

function Base.show(io::IO, d::AbstractAffineDimensions)
    if d.symbol != :nothing
        print(io, d.symbol)
    else
        print(io, "AffineDimensions(scale=", affine_scale(d), ", offset=", affine_offset(d), ", basedim=", affine_base_dim(d), ")")
    end
end

function assert_no_offset(d::AffineDimensions)
    if !iszero(affine_offset(d))
        throw(AffineOffsetError(d))
    end
end

"""
    uexpand(q::Q) where {T,R,D<:AbstractAffineDimensions{R},Q<:UnionAbstractQuantity{T,D}}

Expand the affine units in a quantity to their base SI form (with `Dimensions`).
"""
function uexpand(q::Q) where {T,R,D<:AbstractAffineDimensions{R},Q<:UnionAbstractQuantity{T,D}}
    return _explicit_convert(with_type_parameters(Q, T, Dimensions{R}), q)
end
uexpand(q::QuantityArray{T,N,D}) where {T,N,D<:AbstractAffineDimensions} = uexpand.(q)

for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        # Dimensions to AffineDimensions
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,Q<:$type{T,AffineDimensions}}
            return convert(with_type_parameters(Q, T, AffineDimensions{DEFAULT_DIM_BASE_TYPE}), q)
        end

        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,R,Q<:$type{T,AffineDimensions{R}}}
            dims = AffineDimensions{R}(scale=1, offset=0, basedim=dimension(q))
            return constructorof(Q)(convert(T, ustrip(q)), dims)
        end

        # AffineDimensions to Dimensions (explicit)
        function _explicit_convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}) where {T,D<:Dimensions,Q<:$type{T,D}}
            d = dimension(q)
            v = ustrip(q) * affine_scale(d) + affine_offset(d)
            return constructorof(Q)(convert(T, v), affine_base_dim(d))
        end

        # AffineDimensions to Dimensions (implicit - fails if offset ≠ 0)
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}) where {T,D<:Dimensions,Q<:$type{T,D}}
            assert_no_offset(dimension(q))
            return _explicit_convert(Q, q)
        end
    end
end

function Base.promote_rule(::Type{AffineDimensions{R1}}, ::Type{AffineDimensions{R2}}) where {R1,R2}
    return AffineDimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{AffineDimensions{R1}}, ::Type{Dimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{Dimensions{R1}}, ::Type{AffineDimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{SymbolicDimensions{R1}}, ::Type{AffineDimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
function Base.promote_rule(::Type{AffineDimensions{R1}}, ::Type{SymbolicDimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end

"""
    uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractAffineDimensions}, q::UnionAbstractQuantity{<:Any, <:Dimensions})

You may also convert to a quantity expressed in affine units.
"""
function uconvert(qout::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q::UnionAbstractQuantity{<:Any,<:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    dout = dimension(qout)
    dimension(q) == affine_base_dim(dout) || throw(DimensionError(q, qout))
    vout = (ustrip(q) - affine_offset(dout)) / affine_scale(dout)
    return new_quantity(typeof(q), vout, dout)
end

function uconvert(qout::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q::QuantityArray{<:Any,<:Any,<:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    dout = dimension(qout)
    dimension(q) == affine_base_dim(dout) || throw(DimensionError(q, qout))
    stripped_q = ustrip(q)
    offset = affine_offset(dout)
    scale = affine_scale(dout)
    vout = @. (stripped_q - offset) / scale
    return QuantityArray(vout, dout, quantity_type(q))
end

# Generic conversions through uexpand
function uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractSymbolicDimensions}, qin::AbstractQuantityOrArray{<:Any,<:AbstractAffineDimensions})
    uconvert(qout, uexpand(qin))
end
function uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}, qin::AbstractQuantityOrArray{<:Any,<:AbstractSymbolicDimensions})
    uconvert(qout, uexpand(qin))
end
function uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}, qin::AbstractQuantityOrArray{<:Any,<:AbstractAffineDimensions})
    uconvert(qout, uexpand(qin))
end

for (op, combine) in ((:+, :*), (:-, :/))
    @eval function map_dimensions(op::typeof($op), args::AffineDimensions...)
        map(assert_no_offset, args)
        return AffineDimensions(
            scale=($combine)(map(affine_scale, args)...),
            offset=zero(Float64),
            basedim=map_dimensions(op, map(affine_base_dim, args)...) 
        )
    end
end

# This is required because /(x::Number) results in an error, so it needs to be cased out to inv
function map_dimensions(op::typeof(-), d::AffineDimensions) 
    assert_no_offset(d)
    return AffineDimensions(
        scale=inv(affine_scale(d)),
        offset=zero(Float64),
        basedim=map_dimensions(op, affine_base_dim(d))
    )
end

# This is needed for parsing out exponentials in string macros
function map_dimensions(fix1::Base.Fix1{typeof(*)}, l::AffineDimensions{R}) where {R}
    assert_no_offset(l)
    return AffineDimensions(
        scale=affine_scale(l)^fix1.x,
        offset=zero(Float64),
        basedim=map_dimensions(fix1, affine_base_dim(l))
    )
end

# Helper function for conversions
function _no_offset_expand(q::Q) where {T,R,Q<:UnionAbstractQuantity{T,<:AbstractAffineDimensions{R}}}
    return convert(with_type_parameters(Q, T, Dimensions{R}), q)
end

# Addition expands
function Base.:(+)(q1::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q2::UnionAbstractQuantity{<:Any,<:AffineDimensions})
    return _no_offset_expand(q1) + _no_offset_expand(q2)
end

# Subtraction
function Base.:(-)(q1::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q2::UnionAbstractQuantity{<:Any,<:AffineDimensions})
    if dimension(q1) == dimension(q2)
        return uexpand(q1) - uexpand(q2)
    else
        return _no_offset_expand(q1) - _no_offset_expand(q2)
    end
end

for op in (:(==), :(≈))
    @eval begin
        Base.$op(q1::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q2::UnionAbstractQuantity{<:Any,<:AffineDimensions}) = $op(uexpand(q1), uexpand(q2))
        function Base.$op(d1::AffineDimensions, d2::AffineDimensions)
            $op(affine_base_dim(d1), affine_base_dim(d2)) &&
                $op(affine_scale(d1), affine_scale(d2)) &&
                $op(affine_offset(d1), affine_offset(d2))
        end
    end
end

const DEFAULT_AFFINE_QUANTITY_TYPE = with_type_parameters(DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE, AffineDimensions{DEFAULT_DIM_BASE_TYPE})

module AffineUnits
    using DispatchDoctor: @unstable

    import ..affine_scale, ..affine_offset, ..affine_base_dim, ..dimension
    import ..ustrip, ..ustripexpand, ..constructorof, ..DEFAULT_AFFINE_QUANTITY_TYPE
    import ..DEFAULT_DIM_TYPE, ..DEFAULT_VALUE_TYPE, ..DEFAULT_DIM_BASE_TYPE
    import ..Units: UNIT_SYMBOLS, UNIT_VALUES
    import ..Constants: CONSTANT_SYMBOLS, CONSTANT_VALUES
    import ..Constants
    import ..Quantity, ..INDEX_TYPE, ..AbstractDimensions, ..AffineDimensions, ..UnionAbstractQuantity
    import ..WriteOnceReadMany, ..SymbolicUnits.as_quantity

    # Register a new affine unit
    function _make_affine_unit(q::Q, symbol::Symbol=:nothing) where {T,R,D<:AbstractDimensions{R},Q<:UnionAbstractQuantity{T,D}}
        return constructorof(Q)(one(T), AffineDimensions{R}(scale=ustripexpand(q), offset=0.0, basedim=dimension(q), symbol=symbol))
    end

    const AFFINE_UNIT_SYMBOLS = WriteOnceReadMany(deepcopy(UNIT_SYMBOLS))
    const AFFINE_UNIT_VALUES  = WriteOnceReadMany(map(_make_affine_unit, UNIT_VALUES, UNIT_SYMBOLS))
    const AFFINE_UNIT_MAPPING = WriteOnceReadMany(Dict(s => INDEX_TYPE(i) for (i, s) in enumerate(AFFINE_UNIT_SYMBOLS)))

    function update_external_affine_unit(name::Symbol, q::UnionAbstractQuantity{<:Any,<:AffineDimensions{R}}) where {R}
        ind = get(AFFINE_UNIT_MAPPING, name, INDEX_TYPE(0))
        if !iszero(ind)
            @warn "unit $(name) already exists, skipping"
            return nothing
        end

        dims = dimension(q)
        d_sym = AffineDimensions{DEFAULT_DIM_BASE_TYPE}(
            scale=affine_scale(dims),
            offset=affine_offset(dims),
            basedim=affine_base_dim(dims),
            symbol=(dims.symbol == :nothing) ? name : dims.symbol
        )

        q_sym = constructorof(DEFAULT_AFFINE_QUANTITY_TYPE)(ustrip(q), d_sym)

        push!(AFFINE_UNIT_SYMBOLS, name)
        push!(AFFINE_UNIT_VALUES, q_sym)
        AFFINE_UNIT_MAPPING[name] = lastindex(AFFINE_UNIT_SYMBOLS)
        return nothing
    end
    function update_external_affine_unit(name::Symbol, q::UnionAbstractQuantity)
        return update_external_affine_unit(name, _make_affine_unit(q, name))
    end
    function update_external_affine_unit(name::Symbol, d::AbstractDimensions)
        return update_external_affine_unit(name, Quantity(DEFAULT_VALUE_TYPE(1.0), d))
    end
    function update_external_affine_unit(d::AffineDimensions)  
        d.symbol != :nothing || error("Cannot register affine dimension if symbol is :nothing")
        return update_external_affine_unit(d.symbol, d)
    end

    """
        aff_uparse(s::AbstractString)

    Affine unit parsing function. This works similarly to `uparse`,
    but uses `AffineDimensions` instead of `Dimensions`, and permits affine units such
    as `°C` and `°F`. You may also refer to regular units such as `m` or `s`.
    """
    function aff_uparse(s::AbstractString)
        ex = map_to_scope(Meta.parse(s))
        ex = :($as_quantity($ex))
        return eval(ex)::DEFAULT_AFFINE_QUANTITY_TYPE
    end

    as_quantity(q::DEFAULT_AFFINE_QUANTITY_TYPE) = q

    # String parsing helpers
    @unstable function map_to_scope(ex::Expr)
        if ex.head != :call
            throw(ArgumentError("Unexpected expression: $ex. Only `:call` is expected."))
        end
        ex.args[2:end] = map(map_to_scope, ex.args[2:end])
        return ex
    end

    function map_to_scope(sym::Symbol)
        sym in AFFINE_UNIT_SYMBOLS || throw(ArgumentError("Symbol $sym not found in `AffineUnits`."))
        return lookup_unit(sym)
    end

    map_to_scope(ex) = ex

    function lookup_unit(ex::Symbol)
        i = findfirst(==(ex), AFFINE_UNIT_SYMBOLS)::Int
        return AFFINE_UNIT_VALUES[i]
    end

    # Register standard temperature units
    let
        K  = Quantity(1.0, temperature=1)
        °C = Quantity(1.0, AffineDimensions(scale=1.0, offset=273.15*K, basedim=K, symbol=:°C))
        °F = Quantity(1.0, AffineDimensions(scale=5/9, offset=(-160/9)°C, basedim=°C, symbol=:°F))
        update_external_affine_unit(dimension(°C))
        update_external_affine_unit(:degC, dimension(°C))
        update_external_affine_unit(dimension(°F))
        update_external_affine_unit(:degF, dimension(°F))
    end
    # Register unit symbols as exportable constants
    for (name, val) in zip(AFFINE_UNIT_SYMBOLS, AFFINE_UNIT_VALUES)
        @eval begin
            const $name = $val
        end
    end
end

import .AffineUnits: aff_uparse, update_external_affine_unit

"""
    ua"[unit expression]"

Affine unit parsing macro. This works similarly to `u"[unit expression]"`, but uses 
`AffineDimensions` instead of `Dimensions`, and permits affine units such
as `°C` and `°F`. You may also refer to regular units such as `m` or `s`.

!!! warning
    This is an experimental feature and may change in the future.
"""
macro ua_str(s)
    ex = AffineUnits.map_to_scope(Meta.parse(s))
    ex = :($(AffineUnits.as_quantity)($ex))
    return esc(ex)
end
