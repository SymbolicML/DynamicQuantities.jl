import Compat: allequal

function map_dimensions(f::F, args::AbstractDimensions...) where {F<:Function}
    dimension_type = promote_type(typeof(args).parameters...)
    dim_names = dimension_names(dimension_type)
    return new_dimensions(
        dimension_type,
        (
            f((getproperty(arg, dim) for arg in args)...)
            for dim in dim_names
        )...
    )
end
@generated function all_dimensions(f::F, args::AbstractDimensions...) where {F<:Function}
    # Test a function over all dimensions
    output = Expr(:&&)
    dimension_type = promote_type(args...)
    for dim in Base.fieldnames(dimension_type)
        f_expr = :(f())
        for i=1:length(args)
            push!(f_expr.args, :(args[$i].$dim))
        end
        push!(output.args, f_expr)
    end
    return output
end

for (type, base_type, _) in ABSTRACT_QUANTITY_TYPES
    @eval Base.convert(::Type{$base_type}, q::$type) = q
end
function Base.convert(::Type{T}, q::UnionAbstractQuantity) where {T<:Number}
    @assert iszero(dimension(q)) "$(typeof(q)): $(q) has dimensions! Use `ustrip` instead."
    return convert(T, ustrip(q))
end
function Base.promote_rule(::Type{Dimensions{R1}}, ::Type{Dimensions{R2}}) where {R1,R2}
    return Dimensions{promote_type(R1,R2)}
end
for (_, _, concrete_type) in ABSTRACT_QUANTITY_TYPES
    @eval function Base.promote_rule(::Type{<:$concrete_type{T1,D1}}, ::Type{<:$concrete_type{T2,D2}}) where {T1,T2,D1,D2}
        return $concrete_type{promote_type(T1,T2),promote_type(D1,D2)}
    end
end

function Base.promote_rule(::Type{<:Quantity{T1,D1}}, ::Type{<:GenericQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:GenericQuantity{T1,D1}}, ::Type{<:Quantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:GenericQuantity{T1,D1}}, ::Type{<:RealQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:RealQuantity{T1,D1}}, ::Type{<:GenericQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return GenericQuantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:Quantity{T1,D1}}, ::Type{<:RealQuantity{T2,D2}}) where {T1,T2,D1,D2}
    return Quantity{promote_type(T1,T2),promote_type(D1,D2)}
end
function Base.promote_rule(::Type{<:RealQuantity{T1,D1}}, ::Type{<:Quantity{T2,D2}}) where {T1,T2,D1,D2}
    return Quantity{promote_type(T1,T2),promote_type(D1,D2)}
end

# Define promotion rules for all basic numeric types, individually.
# We don't want to define an opinionated promotion on <:Number,
# or even <:AbstractFloat, as it could conflict with other
# abstract number packages which may try to do the same thing.
# (which would lead to ambiguities)
const BASE_NUMERIC_TYPES = Union{
    Int8, UInt8, Int16, UInt16, Int32, UInt32,
    Int64, UInt64, Int128, UInt128, Float16, Float32,
    Float64, BigInt, ComplexF16, ComplexF32,
    ComplexF64, Complex{BigFloat}, Rational{Int8}, Rational{UInt8},
    Rational{Int16}, Rational{UInt16}, Rational{Int32}, Rational{UInt32},
    Rational{Int64}, Rational{UInt64}, Rational{Int128}, Rational{UInt128},
    Rational{BigInt},
}
# The following types require explicit promotion,
# as putting them in a union type creates different ambiguities
const AMBIGUOUS_NUMERIC_TYPES = (Bool, BigFloat)

for (type, _, _) in ABSTRACT_QUANTITY_TYPES 
    @eval begin
        function Base.convert(::Type{Q}, x::BASE_NUMERIC_TYPES) where {T,D,Q<:$type{T,D}}
            return new_quantity(Q, convert(T, x), D())
        end
        function Base.promote_rule(::Type{Q}, ::Type{T2}) where {T,D,Q<:$type{T,D},T2<:BASE_NUMERIC_TYPES}
            return with_type_parameters(promote_quantity(Q, T2), promote_type(T, T2), D)
        end
        function Base.promote_rule(::Type{T2}, ::Type{Q}) where {T,D,Q<:$type{T,D},T2<:BASE_NUMERIC_TYPES}
            return with_type_parameters(promote_quantity(Q, T2), promote_type(T, T2), D)
        end
    end
    for numeric_type in AMBIGUOUS_NUMERIC_TYPES
        @eval begin
            function Base.convert(::Type{Q}, x::$numeric_type) where {T,D,Q<:$type{T,D}}
                return new_quantity(Q, convert(T, x), D())
            end
            function Base.promote_rule(::Type{Q}, ::Type{$numeric_type}) where {T,D,Q<:$type{T,D}}
                return with_type_parameters(promote_quantity(Q, $numeric_type), promote_type(T, $numeric_type), D)
            end
            function Base.promote_rule(::Type{$numeric_type}, ::Type{Q}) where {T,D,Q<:$type{T,D}}
                return with_type_parameters(promote_quantity(Q, $numeric_type), promote_type(T, $numeric_type), D)
            end
        end
    end
end

"""
    promote_except_value(q1::UnionAbstractQuantity, q2::UnionAbstractQuantity)

This applies a promotion to the quantity type, and the dimension type,
but *not* the value type. This is necessary because sometimes we would
want to multiply a quantity array with a scalar quantity, and wish to use
promotion on the quantity type itself, but don't want to promote to a
single value type.
"""
@inline function promote_except_value(q1::Q1, q2::Q2) where {T1,D1,T2,D2,Q1<:UnionAbstractQuantity{T1,D1},Q2<:UnionAbstractQuantity{T2,D2}}
    Q = promote_type(Q1, Q2)
    D = promote_type(D1, D2)

    Q1_out = with_type_parameters(Q, T1, D)
    Q2_out = with_type_parameters(Q, T2, D)
    return convert(Q1_out, q1), convert(Q2_out, q2)
end
@inline promote_except_value(q1::Q, q2::Q) where {Q<:UnionAbstractQuantity} = (q1, q2)

Base.keys(d::AbstractDimensions) = dimension_names(typeof(d))
Base.getindex(d::AbstractDimensions, k::Symbol) = getfield(d, k)

@generated function dimension_names_equal(::Type{T1}, ::Type{T2}) where {T1,T2}
    # Needs to be a generated function to ensure hardcoded
    return dimension_names(T1) == dimension_names(T2)
end

# Compatibility with `.*`
Base.size(q::UnionAbstractQuantity) = size(ustrip(q))
Base.length(q::UnionAbstractQuantity) = length(ustrip(q))
Base.axes(q::UnionAbstractQuantity) = axes(ustrip(q))
Base.iterate(qd::UnionAbstractQuantity, maybe_state...) =
    let subiterate=iterate(ustrip(qd), maybe_state...)
        subiterate === nothing && return nothing
        return new_quantity(typeof(qd), subiterate[1], dimension(qd)), subiterate[2]
    end
Base.ndims(::Type{<:UnionAbstractQuantity{T}}) where {T} = ndims(T)
Base.ndims(q::UnionAbstractQuantity) = ndims(ustrip(q))
Base.broadcastable(q::UnionAbstractQuantity) = new_quantity(typeof(q), Base.broadcastable(ustrip(q)), dimension(q))
for (type, _, _) in ABSTRACT_QUANTITY_TYPES
    @eval Base.getindex(q::$type) = new_quantity(typeof(q), getindex(ustrip(q)), dimension(q))
    @eval Base.getindex(q::$type, i::Integer...) = new_quantity(typeof(q), getindex(ustrip(q), i...), dimension(q))
    type == AbstractGenericQuantity &&
        @eval Base.getindex(q::$type, i...) = new_quantity(typeof(q), getindex(ustrip(q), i...), dimension(q))
end
Base.keys(q::UnionAbstractQuantity) = keys(ustrip(q))


# Numeric checks
for op in (:(<=), :(<), :(>=), :(>), :isless, :isgreater),
    (type, base_type, _) in ABSTRACT_QUANTITY_TYPES

    @eval begin
        function Base.$(op)(l::$type, r::$type)
            l, r = promote_except_value(l, r)
            dimension(l) == dimension(r) || throw(DimensionError(l, r))
            return $(op)(ustrip(l), ustrip(r))
        end
        function Base.$(op)(l::$type, r::$base_type)
            iszero(dimension(l)) || throw(DimensionError(l, r))
            return $(op)(ustrip(l), r)
        end
        function Base.$(op)(l::$base_type, r::$type)
            iszero(dimension(r)) || throw(DimensionError(l, r))
            return $(op)(l, ustrip(r))
        end
    end
end
for op in (:isequal, :(==)), (type, base_type, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        function Base.$(op)(l::$type, r::$type)
            l, r = promote_except_value(l, r)
            return $(op)(ustrip(l), ustrip(r)) && dimension(l) == dimension(r)
        end
        function Base.$(op)(l::$type, r::$base_type)
            return $(op)(ustrip(l), r) && iszero(dimension(l))
        end
        function Base.$(op)(l::$base_type, r::$type)
            return $(op)(l, ustrip(r)) && iszero(dimension(r))
        end
    end
end
for op in (:(<=), :(<), :(>=), :(>), :isless, :isgreater, :isequal, :(==)),
    (t1, _, _) in ABSTRACT_QUANTITY_TYPES,
    (t2, _, _) in ABSTRACT_QUANTITY_TYPES

    t1 == t2 && continue

    @eval function Base.$(op)(l::$t1, r::$t2)
        return $(op)(promote_except_value(l, r)...)
    end
end
# Define isapprox:
for (type, base_type, _) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        function Base.isapprox(l::$type, r::$type; kws...)
            dimension(l) == dimension(r) || throw(DimensionError(l, r))
            return isapprox(ustrip(l), ustrip(r); kws...)
        end
        function Base.isapprox(l::$base_type, r::$type; kws...)
            iszero(dimension(r)) || throw(DimensionError(l, r))
            return isapprox(l, ustrip(r); kws...)
        end
        function Base.isapprox(l::$type, r::$base_type; kws...)
            iszero(dimension(l)) || throw(DimensionError(l, r))
            return isapprox(ustrip(l), r; kws...)
        end
    end
    for (type2, _, _) in ABSTRACT_QUANTITY_TYPES

        type == type2 && continue

        @eval function Base.isapprox(l::$type, r::$type2; kws...)
            return isapprox(promote_except_value(l, r)...; kws...)
        end
    end
end


# Simple flags:
for f in (
    :iszero, :isfinite, :isinf, :isnan, :isreal, :signbit,
    :isempty, :iseven, :isodd, :isinteger, :ispow2
)
    @eval Base.$f(q::UnionAbstractQuantity) = $f(ustrip(q))
end
Base.iszero(d::AbstractDimensions) = all_dimensions(iszero, d)
Base.:(==)(l::AbstractDimensions, r::AbstractDimensions) = all_dimensions(==, l, r)


# Base.one, typemin, typemax
for f in (:one, :typemin, :typemax)
    @eval begin
        Base.$f(::Type{Q}) where {T,D,Q<:UnionAbstractQuantity{T,D}} = new_quantity(Q, $f(T), D)
        Base.$f(::Type{Q}) where {T,Q<:UnionAbstractQuantity{T}} = $f(with_type_parameters(Q, T, DEFAULT_DIM_TYPE))
        Base.$f(::Type{Q}) where {Q<:UnionAbstractQuantity} = $f(with_type_parameters(Q, DEFAULT_VALUE_TYPE, DEFAULT_DIM_TYPE))
    end
    if f == :one  # Return empty dimensions, as should be multiplicative identity.
        @eval Base.$f(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, $f(ustrip(q)), one(dimension(q)))
    else
        @eval Base.$f(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, $f(ustrip(q)), dimension(q))
    end
end
Base.one(::Type{D}) where {D<:AbstractDimensions} = D()
Base.one(::D) where {D<:AbstractDimensions} = one(D)

# Additive identities (zero)
Base.zero(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, zero(ustrip(q)), dimension(q))
Base.zero(::AbstractDimensions) = error("There is no such thing as an additive identity for a `AbstractDimensions` object, as + is only defined for `UnionAbstractQuantity`.")
Base.zero(::Type{<:UnionAbstractQuantity}) = error("Cannot create an additive identity for a `UnionAbstractQuantity` type, as the dimensions are unknown. Please use `zero(::UnionAbstractQuantity)` instead.")
Base.zero(::Type{<:AbstractDimensions}) = error("There is no such thing as an additive identity for a `AbstractDimensions` type, as + is only defined for `UnionAbstractQuantity`.")

# Dimensionful 1 (oneunit)
Base.oneunit(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, oneunit(ustrip(q)), dimension(q))
Base.oneunit(::AbstractDimensions) = error("There is no such thing as a dimensionful 1 for a `AbstractDimensions` object, as + is only defined for `UnionAbstractQuantity`.")
Base.oneunit(::Type{<:UnionAbstractQuantity}) = error("Cannot create a dimensionful 1 for a `UnionAbstractQuantity` type without knowing the dimensions. Please use `oneunit(::UnionAbstractQuantity)` instead.")
Base.oneunit(::Type{<:AbstractDimensions}) = error("There is no such thing as a dimensionful 1 for a `AbstractDimensions` type, as + is only defined for `UnionAbstractQuantity`.")

Base.show(io::IO, d::AbstractDimensions) =
    let tmp_io = IOBuffer()
        for k in filter(k -> !iszero(d[k]), keys(d))
            print(tmp_io, dimension_name(d, k))
            isone(d[k]) || pretty_print_exponent(tmp_io, d[k])
            print(tmp_io, " ")
        end
        s = String(take!(tmp_io))
        s = replace(s, r"^\s*" => "")
        s = replace(s, r"\s*$" => "")
        print(io, s)
    end
Base.show(io::IO, q::UnionAbstractQuantity{<:Real}) = print(io, ustrip(q), " ", dimension(q))
Base.show(io::IO, q::UnionAbstractQuantity) = print(io, "(", ustrip(q), ") ", dimension(q))

function dimension_name(::AbstractDimensions, k::Symbol)
    default_dimensions = (length="m", mass="kg", time="s", current="A", temperature="K", luminosity="cd", amount="mol")
    return get(default_dimensions, k, string(k))
end

string_rational(x) = isinteger(x) ? string(round(Int, x)) : string(x)
pretty_print_exponent(io::IO, x) = print(io, to_superscript(string_rational(x)))

tryrationalize(::Type{R}, x::R) where {R} = x
tryrationalize(::Type{R}, x::Union{Rational,Integer}) where {R} = convert(R, x)
tryrationalize(::Type{R}, x) where {R} = isinteger(x) ? convert(R, round(Int, x)) : convert(R, rationalize(Int, x))

Base.showerror(io::IO, e::DimensionError) = print(io, "DimensionError: ", e.q1, " and ", e.q2, " have incompatible dimensions")
Base.showerror(io::IO, e::DimensionError{<:Any,Nothing}) = print(io, "DimensionError: ", e.q1, " is not dimensionless")

Base.convert(::Type{Q}, q::UnionAbstractQuantity) where {Q<:UnionAbstractQuantity} = q
Base.convert(::Type{Q}, q::UnionAbstractQuantity) where {T,Q<:UnionAbstractQuantity{T}} = new_quantity(Q, convert(T, ustrip(q)), dimension(q))
Base.convert(::Type{Q}, q::UnionAbstractQuantity) where {T,D,Q<:UnionAbstractQuantity{T,D}} = new_quantity(Q, convert(T, ustrip(q)), convert(D, dimension(q)))

Base.convert(::Type{D}, d::AbstractDimensions) where {D<:AbstractDimensions} = d
Base.convert(::Type{D}, d::AbstractDimensions) where {R,D<:AbstractDimensions{R}} = D(d)

Base.copy(d::D) where {D<:AbstractDimensions} = map_dimensions(copy, d)
Base.copy(q::Q) where {Q<:UnionAbstractQuantity} = new_quantity(Q, copy(ustrip(q)), copy(dimension(q)))

"""
    ustrip(q::AbstractQuantity)
    ustrip(q::AbstractGenericQuantity)

Remove the units from a quantity.
"""
@inline ustrip(q::UnionAbstractQuantity) = q.value
ustrip(::AbstractDimensions) = error("Cannot remove units from an `AbstractDimensions` object.")
@inline ustrip(q) = q

"""
    dimension(q::AbstractQuantity)
    dimension(q::AbstractGenericQuantity)

Get the dimensions of a quantity, returning an `AbstractDimensions` object.
"""
dimension(q::UnionAbstractQuantity) = q.dimensions
dimension(d::AbstractDimensions) = d
dimension(aq::AbstractArray{<:UnionAbstractQuantity}) = allequal(dimension.(aq)) ? dimension(first(aq)) : throw(DimensionError(aq[begin], aq[begin+1:end]))

"""
    ulength(q::AbstractQuantity)
    ulength(q::AbstractGenericQuantity)
    ulength(d::AbstractDimensions)

Get the length dimension of a quantity (e.g., meters^(ulength)).
"""
ulength(q::UnionAbstractQuantity) = ulength(dimension(q))
ulength(d::AbstractDimensions) = d.length

"""
    umass(q::AbstractQuantity)
    umass(q::AbstractGenericQuantity)
    umass(d::AbstractDimensions)

Get the mass dimension of a quantity (e.g., kg^(umass)).
"""
umass(q::UnionAbstractQuantity) = umass(dimension(q))
umass(d::AbstractDimensions) = d.mass

"""
    utime(q::AbstractQuantity)
    utime(q::AbstractGenericQuantity)
    utime(d::AbstractDimensions)

Get the time dimension of a quantity (e.g., s^(utime))
"""
utime(q::UnionAbstractQuantity) = utime(dimension(q))
utime(d::AbstractDimensions) = d.time

"""
    ucurrent(q::AbstractQuantity)
    ucurrent(q::AbstractGenericQuantity)
    ucurrent(d::AbstractDimensions)

Get the current dimension of a quantity (e.g., A^(ucurrent)).
"""
ucurrent(q::UnionAbstractQuantity) = ucurrent(dimension(q))
ucurrent(d::AbstractDimensions) = d.current

"""
    utemperature(q::AbstractQuantity)
    utemperature(q::AbstractGenericQuantity)
    utemperature(d::AbstractDimensions)

Get the temperature dimension of a quantity (e.g., K^(utemperature)).
"""
utemperature(q::UnionAbstractQuantity) = utemperature(dimension(q))
utemperature(d::AbstractDimensions) = d.temperature

"""
    uluminosity(q::AbstractQuantity)
    uluminosity(q::AbstractGenericQuantity)
    uluminosity(d::AbstractDimensions)

Get the luminosity dimension of a quantity (e.g., cd^(uluminosity)).
"""
uluminosity(q::UnionAbstractQuantity) = uluminosity(dimension(q))
uluminosity(d::AbstractDimensions) = d.luminosity

"""
    uamount(q::AbstractQuantity)
    uamount(q::AbstractGenericQuantity)
    uamount(d::AbstractDimensions)

Get the amount dimension of a quantity (e.g., mol^(uamount)).
"""
uamount(q::UnionAbstractQuantity) = uamount(dimension(q))
uamount(d::AbstractDimensions) = d.amount
