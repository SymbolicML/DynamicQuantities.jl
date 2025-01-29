#=
ToDo:
    (1) Dimensional parsing

    (2) Unit registration




using DynamicQuantities

import DynamicQuantities.Units: UNIT_SYMBOLS, UNIT_MAPPING, UNIT_VALUES
import DynamicQuantities.ABSTRACT_QUANTITY_TYPES
import DynamicQuantities: DEFAULT_DIM_BASE_TYPE, DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE
import DynamicQuantities: WriteOnceReadMany, with_type_parameters, constructorof, isinteger, uexpand, uconvert, new_quantity
import DynamicQuantities.Constants: CONSTANT_SYMBOLS, CONSTANT_MAPPING, CONSTANT_VALUES
import DynamicQuantities: disambiguate_constant_symbol, ALL_MAPPING, ALL_VALUES
=#


const INDEX_TYPE = UInt16
const AbstractQuantityOrArray{T,D} = Union{Quantity{T,D}, QuantityArray{T,<:Any,D}}

abstract type AbstractAffineDimensions{R} <: AbstractDimensions{R} end

const AffineOrSymbolicDimensions{R} = Union{AbstractAffineDimensions{R}, AbstractSymbolicDimensions{R}}

@kwdef struct AffineDimensions{R} <: AbstractAffineDimensions{R}
    scale::Float64
    offset::Float64
    basedim::Dimensions{R}
end

function AffineDimensions(scale::Real, offset::Real, d::Dimensions{R}) where {R}
    return AffineDimensions{R}(scale, offset, d)
end

function AffineDimensions(scale::Real, offset::Real, q::UnionAbstractQuantity{T,<:AbstractDimensions{R}}) where {T,R}
    return AffineDimensions{R}(scale, offset, dimension(q))
end

function AffineDimensions(s::Real, o::Real, dims::AffineDimensions{R}) where {R}
    new_s = s*scale(dims)
    new_o = offset(dims) + o
    return AffineDimensions{R}(new_s, new_o, basedim(dims))
end

AffineDimensions(d::Dimensions{R}) where R = AffineDimenions{R}(scale=0.0, offset=0.0, basedim=d)

scale(d::AffineDimensions)  = d.scale
offset(d::AffineDimensions) = d.offset
basedim(d::AffineDimensions) = d.basedim

with_type_parameters(::Type{<:AffineDimensions}, ::Type{R}) where {R} = AffineDimensions{R}
constructorof(::Type{AffineDimensions}) = AffineDimensions{DEFAULT_DIM_BASE_TYPE}
constructorof(::Type{AffineDimensions{R}}) where R = AffineDimensions{R}

function Base.show(io::IO, d::AbstractAffineDimensions)
    addsign = ifelse(offset(d)<0, " - " , " + ")
    print(io, " ", scale(d), "(",basedim(d),")", addsign, offset(d))
end

assert_no_offset(d::AffineDimensions) = iszero(offset(d)) || throw(AssertionError("AffineDimensions $(d) has a non-zero offset, implicit conversion is not allowed due to ambiguity. Use uexpand(x) to explicitly convert"))

"""
affine(q::Q) where {T, R, D<:AbstractDimensions{R}, Q<:UnionAbstractQuantity{T,D}}

Converts a quantity to its nearest affine representation (with scale=1.0 and offset=0.0)
"""
function affine(q::Q) where {T, R, D<:AbstractDimensions{R}, Q<:UnionAbstractQuantity{T,D}}
    return convert(with_type_parameters(Q, T, AffineDimensions{R}), q)
end

#Conversions
for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,Q<:$type{T,AffineDimensions}}
            return convert(with_type_parameters(Q, T, AffineDimensions{DEFAULT_DIM_BASE_TYPE}), q)
        end

        #Conversion of (AbstractQuantity){T,Dimensions{R}} to (AbstractQuantity){T,AffineDimensions{R}}
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:Dimensions}) where {T,R,Q<:$type{T,AffineDimensions{R}}}
            dims = AffineDimensions{R}(scale=1, offset=0, basedim=dimension(q))
            return constructorof(Q)(convert(T, ustrip(q)), dims)
        end

        #Forced conversion of (AbstractQuantity){T,R<:AffineDimensions} to (AbstractQuantity){T,R<:Dimensions} (zero offset requirement overridden)
        function force_convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}) where {T,D<:Dimensions,Q<:$type{T,D}}
            d = dimension(q)
            v = ustrip(q)*scale(d) + offset(d)
            return constructorof(Q)(convert(T, v), basedim(d))
        end

        #Conversion of (AbstractQuantity){T,R<:AffineDimensions} to (AbstractQuantity){T,R<:Dimensions}
        function Base.convert(::Type{Q}, q::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}) where {T,D<:Dimensions,Q<:$type{T,D}}
            assert_no_offset(dimension(q))
            return force_convert(Q, q)
        end
    end
end

#Promotion rules
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

#Constants are not imported
const AFFINE_SYMBOLS = WriteOnceReadMany([UNIT_SYMBOLS...])
const AFFINE_VALUES  = WriteOnceReadMany(affine.([UNIT_VALUES...]))
const AFFINE_MAPPING = WriteOnceReadMany(Dict(s => INDEX_TYPE(i) for (i, s) in enumerate(AFFINE_SYMBOLS)))


"""
uexpand(q::Q) where {T, R, D<:AbstractAffineDimensions{R}, Q<:UnionAbstractQuantity{T,D}}

Expand the affine units in a quantity to their base SI form. In other words, this converts a quantity with AbstractAffineDimensions   
to one with Dimensions. The opposite of this function is uconvert, for converting to specific symbolic units, or, e.g.,
convert(Quantity{<:Any,<:AbstractSymbolicDimensions}, q), for assuming SI units as the output symbols.
"""
function uexpand(q::Q) where {T, R, D<:AbstractAffineDimensions{R}, Q<:UnionAbstractQuantity{T,D}}
    return force_convert(with_type_parameters(Q, T, Dimensions{R}), q)
end

# Conversions for Dimensions |> AffineDimenions =====================================================================================
"""
    uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractAffineDimensions}, q::UnionAbstractQuantity{<:Any, <:Dimensions})

Convert a quantity `q` with base SI units to the affine units of `qout`, for `q` and `qout` with compatible units.
You can also use `|>` as a shorthand for `uconvert`
"""
function uconvert(qout::UnionAbstractQuantity{<:Any, <:AffineDimensions}, q::UnionAbstractQuantity{<:Any, <:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    dout = dimension(qout)
    dimension(q) == basedim(dout) || throw(DimensionError(q, qout_expanded))
    vout = (ustrip(q)-offset(dout))/scale(dout)
    return new_quantity(typeof(q), vout, dout)
end

function uconvert(qout::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q::QuantityArray{<:Any,<:Any,<:Dimensions})
    @assert isone(ustrip(qout)) "You passed a quantity with a non-unit value to uconvert."
    dout = dimension(qout)
    dimension(q) == basedim(dout) || throw(DimensionError(q, qout_expanded))
    vout = (ustrip(q) .- offset(dout))./scale(dout)
    return QuantityArray(vout, dout, quantity_type(q))
end

# Conversions for AffineOrSymbolicDimensions |> SymbolicDimensions =======================================================
function uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractSymbolicDimensions}, qin::AbstractQuantityOrArray{<:Any, <:AffineOrSymbolicDimensions})
    uconvert(qout, uexpand(qin))
end

function uconvert(qout::UnionAbstractQuantity{<:Any,<:AbstractAffineDimensions}, qin::AbstractQuantityOrArray{<:Any, <:AffineOrSymbolicDimensions})
    uconvert(qout, uexpand(qin))
end

# Conversions for AffineDimensions |> AffineDimensions =======================================================
function uconvert(qout::UnionAbstractQuantity{<:Any, <:AbstractAffineDimensions}, qin::AbstractQuantityOrArray{<:Any, <:AffineDimensions})
    uconvert(qout, uexpand(qin))
end

# Multiplication and division of AffineDimensions ===============================================================
function Base.:*(l::AffineDimensions, r::AffineDimensions) 
    assert_no_offset(l)
    assert_no_offset(r)
    return AffineDimensions(
        scale  = scale(l)*scale(r),
        offset = offset(l),
        basedim = basedim(l)*basedim(r)
    )
end

function Base.:/(l::AffineDimensions, r::AffineDimensions) 
    assert_no_offset(l)
    assert_no_offset(r)
    return AffineDimensions(
        scale  = scale(l)/scale(r),
        offset = offset(l),
        basedim = basedim(l)/basedim(r)
    )
end

# Exponentiation ===============================================================
function Base.:^(l::AffineDimensions{R}, r::Number) where {R}
    assert_no_offset(l)
    return AffineDimensions(
        scale = scale(l)^r,
        offset = offset(l),
        basedim = map_dimensions(Base.Fix1(*, tryrationalize(R, r)), basedim(l))
    )
end

# Operations on self-values ======================================================================================
function _scale_expand(q::Q) where {T, R, D<:AbstractAffineDimensions{R}, Q<:UnionAbstractQuantity{T,D}}
    return convert(with_type_parameters(Q, T, Dimensions{R}), q)
end

#Addition will return Quantity{T, Dimensions}
Base.:+(q1::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q2::UnionAbstractQuantity{<:Any,<:AffineDimensions}) = _scale_expand(q1) + _scale_expand(q2)

#Subtraction will return Quantity{T, Dimensions}, in special cases, differences between offsetted AffineDimensions is allowed as offsets cancel out
function Base.:-(q1::UnionAbstractQuantity{<:Any,<:AffineDimensions}, q2::UnionAbstractQuantity{<:Any,<:AffineDimensions})
    if dimension(q1) == dimension(q2)
        return uexpand(q1) - uexpand(q2)
    else
        return _scale_expand(q1) - _scale_expand(q2)
    end
end

Base.:(==)(q1::UnionAbstractQuantity{<:Any, <:AffineDimensions}, q2::UnionAbstractQuantity{<:Any, <:AffineDimensions}) = uexpand(q1) == uexpand(q2)


# Units are stored using SymbolicDimensionsSingleton
const DEFAULT_AFFINE_QUANTITY_TYPE = with_type_parameters(DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE, AffineDimensions{DEFAULT_DIM_BASE_TYPE})
# However, we output units from `us_str` using SymbolicDimensions, for type stability
const DEFAULT_AFFINE_QUANTITY_OUTPUT_TYPE = with_type_parameters(DEFAULT_QUANTITY_TYPE, DEFAULT_VALUE_TYPE, AffineDimensions{DEFAULT_DIM_BASE_TYPE})


#=
"""
    SymbolicUnits

A separate module where each unit is treated as a separate dimension,
to enable pretty-printing of units.
"""
module AffineUnits

    using DispatchDoctor: @unstable

    import ..affine
    import ..UNIT_SYMBOLS
    import ..UNIT_VALUES
    import ..CONSTANT_SYMBOLS
    import ..AffineDimensions
    import ..constructorof
    import ..DEFAULT_AFFINE_QUANTITY_TYPE
    import ..DEFAULT_AFFINE_QUANTITY_OUTPUT_TYPE
    import ..DEFAULT_VALUE_TYPE
    import ..DEFAULT_DIM_BASE_TYPE
    import ..WriteOnceReadMany

    const AFFINE_UNIT_SYMBOLS = deepcopy(UNIT_SYMBOLS)
    const AFFINE_UNIT_VALUES  = WriteOnceReadMany{Vector{DEFAULT_AFFINE_QUANTITY_TYPE}}()

    # Used for registering units in current module
    function update_affine_unit_values!(q, symbolic_unit_values = AFFINE_UNIT_VALUES)
        push!(symbolic_unit_values, q)
    end
    update_affine_unit_values!(w::WriteOnceReadMany) = update_affine_unit_values!.(w._raw_data)
    update_affine_unit_values!(UNIT_VALUES)

    
    # Used for registering units in an external module
    function update_external_affine_unit_value(name::Symbol, unit::Quantity)
        push!(AFFINE_UNIT_SYMBOLS, name)
        push!(AFFINE_UNIT_VALUES, affine(unit))
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
    function sym_uparse(s::AbstractString)
        ex = map_to_scope(Meta.parse(s))
        ex = :($as_quantity($ex))
        return copy(eval(ex))::DEFAULT_AFFINE_QUANTITY_OUTPUT_TYPE
    end

    as_quantity(q::DEFAULT_AFFINE_QUANTITY_OUTPUT_TYPE) = q
    as_quantity(x::Number) = convert(DEFAULT_AFFINE_QUANTITY_OUTPUT_TYPE, x)
    as_quantity(x) = error("Unexpected type evaluated: $(typeof(x))")

    @unstable function map_to_scope(ex::Expr)
        if !(ex.head == :call) && !(ex.head == :. && ex.args[1] == :Constants)
            throw(ArgumentError("Unexpected expression: $ex. Only `:call` and `:.` (for `SymbolicConstants`) are expected."))
        end
        if ex.head == :call
            ex.args[2:end] = map(map_to_scope, ex.args[2:end])
            return ex
        else # if ex.head == :. && ex.args[1] == :Constants
            @assert ex.args[2] isa QuoteNode
            return lookup_constant(ex.args[2].value)
        end
    end
    function map_to_scope(sym::Symbol)
        if sym in UNIT_SYMBOLS
            # return at end
        elseif sym in CONSTANT_SYMBOLS
            throw(ArgumentError("Symbol $sym found in `Constants` but not `Units`. Please use `us\"Constants.$sym\"` instead."))
        else
            throw(ArgumentError("Symbol $sym not found in `Units` or `Constants`."))
        end
        return lookup_unit(sym)
    end
    function map_to_scope(ex)
        return ex
    end
    function lookup_unit(ex::Symbol)
        i = findfirst(==(ex), UNIT_SYMBOLS)::Int
        return as_quantity(AFFINE_UNIT_VALUES[i])
    end

end


import .SymbolicUnits: as_quantity, sym_uparse, SymbolicConstants, map_to_scope

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
    ex = map_to_scope(Meta.parse(s))
    ex = :($as_quantity($ex))
    return esc(ex)
end

=#




