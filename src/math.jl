Base.:*(l::AbstractDimensions, r::AbstractDimensions) = map_dimensions(+, l, r)
Base.:*(l::AbstractQuantity, r::AbstractQuantity) = new_quantity(typeof(l), ustrip(l) * ustrip(r), dimension(l) * dimension(r))
Base.:*(l::AbstractQuantity, r::AbstractDimensions) = new_quantity(typeof(l), ustrip(l), dimension(l) * r)
Base.:*(l::AbstractDimensions, r::AbstractQuantity) = new_quantity(typeof(r), ustrip(r), l * dimension(r))
Base.:*(l::AbstractQuantity, r) = new_quantity(typeof(l), ustrip(l) * r, dimension(l))
Base.:*(l, r::AbstractQuantity) = new_quantity(typeof(r), l * ustrip(r), dimension(r))
Base.:*(l::AbstractDimensions, r) = error("Please use an `AbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")
Base.:*(l, r::AbstractDimensions) = error("Please use an `AbstractQuantity` for multiplication. You used multiplication on types: $(typeof(l)) and $(typeof(r)).")

Base.:/(l::AbstractDimensions, r::AbstractDimensions) = map_dimensions(-, l, r)
Base.:/(l::AbstractQuantity, r::AbstractQuantity) = new_quantity(typeof(l), ustrip(l) / ustrip(r), dimension(l) / dimension(r))
Base.:/(l::AbstractQuantity, r::AbstractDimensions) = new_quantity(typeof(l), ustrip(l), dimension(l) / r)
Base.:/(l::AbstractDimensions, r::AbstractQuantity) = new_quantity(typeof(r), inv(ustrip(r)), l / dimension(r))
Base.:/(l::AbstractQuantity, r) = new_quantity(typeof(l), ustrip(l) / r, dimension(l))
Base.:/(l, r::AbstractQuantity) = l * inv(r)
Base.:/(l::AbstractDimensions, r) = error("Please use an `AbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")
Base.:/(l, r::AbstractDimensions) = error("Please use an `AbstractQuantity` for division. You used division on types: $(typeof(l)) and $(typeof(r)).")

Base.:+(l::AbstractQuantity, r::AbstractQuantity) =
    let
        l, r = dimension_promote(l, r)
        new_quantity(typeof(l), ustrip(l) + ustrip(r), dimension(l))
    end
Base.:-(l::AbstractQuantity) = new_quantity(typeof(l), -ustrip(l), dimension(l))
Base.:-(l::AbstractQuantity, r::AbstractQuantity) = l + (-r)

Base.:+(l::AbstractQuantity, r) =
    let
        l, r = dimension_promote(l, r)
        new_quantity(typeof(l), ustrip(l) + r, dimension(l))
    end
Base.:+(l, r::AbstractQuantity) =
    let
        l, r = dimension_promote(l, r)
        new_quantity(typeof(r), l + ustrip(r), dimension(r))
    end
Base.:-(l::AbstractQuantity, r) = l + (-r)
Base.:-(l, r::AbstractQuantity) = l + (-r)

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

function Base.:^(l::AbstractQuantity{T,D}, r::Integer) where {T,R,D<:AbstractDimensions{R}}
    return new_quantity(typeof(l), ustrip(l)^r, dimension(l)^r)
end
function Base.:^(l::AbstractQuantity{T,D}, r::Number) where {T,R,D<:AbstractDimensions{R}}
    dim_pow = tryrationalize(R, r)
    val_pow = convert(T, dim_pow)
    # Need to ensure we take the numerical power by the rationalized quantity:
    return new_quantity(typeof(l), ustrip(l)^val_pow, dimension(l)^dim_pow)
end
@inline Base.literal_pow(::typeof(^), l::AbstractDimensions, ::Val{p}) where {p} = map_dimensions(Base.Fix1(*, p), l)
@inline Base.literal_pow(::typeof(^), l::AbstractQuantity, ::Val{p}) where {p} = new_quantity(typeof(l), Base.literal_pow(^, ustrip(l), Val(p)), Base.literal_pow(^, dimension(l), Val(p)))

Base.inv(d::AbstractDimensions) = map_dimensions(-, d)
Base.inv(q::AbstractQuantity) = new_quantity(typeof(q), inv(ustrip(q)), inv(dimension(q)))

Base.sqrt(d::AbstractDimensions{R}) where {R} = d^inv(convert(R, 2))
Base.sqrt(q::AbstractQuantity) = new_quantity(typeof(q), sqrt(ustrip(q)), sqrt(dimension(q)))
Base.cbrt(d::AbstractDimensions{R}) where {R} = d^inv(convert(R, 3))
Base.cbrt(q::AbstractQuantity) = new_quantity(typeof(q), cbrt(ustrip(q)), cbrt(dimension(q)))

Base.abs(q::AbstractQuantity) = new_quantity(typeof(q), abs(ustrip(q)), dimension(q))
Base.abs2(q::AbstractQuantity) = new_quantity(typeof(q), abs2(ustrip(q)), dimension(q)^2)
Base.angle(q::AbstractQuantity{T}) where {T<:Complex} = angle(ustrip(q))
