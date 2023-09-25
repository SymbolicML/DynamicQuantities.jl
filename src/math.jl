for (type, base_type) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        Base.:*(l::$type, r::$type) = new_quantity(typeof(l), ustrip(l) * ustrip(r), dimension(l) * dimension(r))
        Base.:/(l::$type, r::$type) = new_quantity(typeof(l), ustrip(l) / ustrip(r), dimension(l) / dimension(r))

        Base.:*(l::$type, r::$base_type) = new_quantity(typeof(l), ustrip(l) * r, dimension(l))
        Base.:/(l::$type, r::$base_type) = new_quantity(typeof(l), ustrip(l) / r, dimension(l))

        Base.:*(l::$base_type, r::$type) = new_quantity(typeof(r), l * ustrip(r), dimension(r))
        Base.:/(l::$base_type, r::$type) = new_quantity(typeof(r), l / ustrip(r), inv(dimension(r)))
    end
end

Base.:*(l::AbstractUnionQuantity, r::AbstractDimensions) = new_quantity(typeof(l), ustrip(l), dimension(l) * r)
Base.:/(l::AbstractUnionQuantity, r::AbstractDimensions) = new_quantity(typeof(l), ustrip(l), dimension(l) / r)
Base.:*(l::AbstractDimensions, r::AbstractUnionQuantity) = new_quantity(typeof(r), ustrip(r), l * dimension(r))
Base.:/(l::AbstractDimensions, r::AbstractUnionQuantity) = new_quantity(typeof(r), inv(ustrip(r)), l / dimension(r))
Base.:*(l::AbstractDimensions, r::AbstractDimensions) = map_dimensions(+, l, r)
Base.:/(l::AbstractDimensions, r::AbstractDimensions) = map_dimensions(-, l, r)

for (type, base_type) in ABSTRACT_QUANTITY_TYPES
    @eval begin
        Base.:+(l::$type, r::$type) =
            let
                dimension(l) == dimension(r) || throw(DimensionError(l, r))
                new_quantity(typeof(l), ustrip(l) + ustrip(r), dimension(l))
            end
        Base.:+(l::$type, r::$base_type) =
            let
                iszero(dimension(l)) || throw(DimensionError(l, r))
                new_quantity(typeof(l), ustrip(l) + r, dimension(l))
            end
        Base.:+(l::$base_type, r::$type) =
            let
                iszero(dimension(r)) || throw(DimensionError(l, r))
                new_quantity(typeof(r), l + ustrip(r), dimension(r))
            end

        Base.:-(l::$type, r::$type) = l + (-r)
        Base.:-(l::$type, r::$base_type) = l + (-r)
        Base.:-(l::$base_type, r::$type) = l + (-r)
    end
end

Base.:-(l::AbstractUnionQuantity) = new_quantity(typeof(l), -ustrip(l), dimension(l))

# More helpful errors:
Base.:*(l::AbstractDimensions, r::Number) = error("Please use an `AbstractUnionQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:*(l::Number, r::AbstractDimensions) = error("Please use an `AbstractUnionQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::AbstractDimensions, r::Number) = error("Please use an `AbstractUnionQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l::Number, r::AbstractDimensions) = error("Please use an `AbstractUnionQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")

# Combining different abstract types
for op in (:*, :/, :+, :-),
    (t1, _) in ABSTRACT_QUANTITY_TYPES,
    (t2, _) in ABSTRACT_QUANTITY_TYPES

    t1 == t2 && continue

    @eval Base.$op(l::$t1, r::$t2) = $op(promote(l, r)...)
end

# We don't promote on the dimension types:
function Base.:^(l::AbstractDimensions{R}, r::Integer) where {R}
    return map_dimensions(Base.Fix1(*, r), l)
end
function Base.:^(l::AbstractDimensions{R}, r::Number) where {R}
    return map_dimensions(Base.Fix1(*, tryrationalize(R, r)), l)
end
# Special forms for small integer powers (will unroll dimension multiplication into repeated additions)
# https://github.com/JuliaLang/julia/blob/b99f251e86c7c09b957a1b362b6408dbba106ff0/base/intfuncs.jl#L332
for (p, ex) in [
    (0, :(one(l))),
    (1, :(l)),
    (2, :(l * l)),
    (3, :(l * l * l)),
    (-1, :(inv(l))),
    (-2, :((i=inv(l); i*i)))
]
    @eval @inline Base.literal_pow(::typeof(^), l::AbstractDimensions, ::Val{$p}) = $ex
end

function Base.:^(l::AbstractUnionQuantity{T,D}, r::Integer) where {T,R,D<:AbstractDimensions{R}}
    return new_quantity(typeof(l), ustrip(l)^r, dimension(l)^r)
end
function Base.:^(l::AbstractUnionQuantity{T,D}, r::Number) where {T,R,D<:AbstractDimensions{R}}
    dim_pow = tryrationalize(R, r)
    val_pow = convert(T, dim_pow)
    # Need to ensure we take the numerical power by the rationalized quantity:
    return new_quantity(typeof(l), ustrip(l)^val_pow, dimension(l)^dim_pow)
end
@inline Base.literal_pow(::typeof(^), l::AbstractDimensions, ::Val{p}) where {p} = map_dimensions(Base.Fix1(*, p), l)
@inline Base.literal_pow(::typeof(^), l::AbstractUnionQuantity, ::Val{p}) where {p} = new_quantity(typeof(l), Base.literal_pow(^, ustrip(l), Val(p)), Base.literal_pow(^, dimension(l), Val(p)))

Base.inv(d::AbstractDimensions) = map_dimensions(-, d)
Base.inv(q::AbstractUnionQuantity) = new_quantity(typeof(q), inv(ustrip(q)), inv(dimension(q)))

Base.sqrt(d::AbstractDimensions{R}) where {R} = d^inv(convert(R, 2))
Base.sqrt(q::AbstractUnionQuantity) = new_quantity(typeof(q), sqrt(ustrip(q)), sqrt(dimension(q)))
Base.cbrt(d::AbstractDimensions{R}) where {R} = d^inv(convert(R, 3))
Base.cbrt(q::AbstractUnionQuantity) = new_quantity(typeof(q), cbrt(ustrip(q)), cbrt(dimension(q)))

Base.abs(q::AbstractUnionQuantity) = new_quantity(typeof(q), abs(ustrip(q)), dimension(q))
Base.abs2(q::AbstractUnionQuantity) = new_quantity(typeof(q), abs2(ustrip(q)), dimension(q)^2)
Base.angle(q::AbstractUnionQuantity{T}) where {T<:Complex} = angle(ustrip(q))
