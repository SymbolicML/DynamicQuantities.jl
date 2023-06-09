Base.:*(l::Dimensions, r::Dimensions) = @map_dimensions(+, l, r)
Base.:*(l::Quantity, r::Quantity) = Quantity(l.value * r.value, l.dimensions * r.dimensions)
Base.:*(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions * r)
Base.:*(l::Dimensions, r::Quantity) = Quantity(r.value, l * r.dimensions)
Base.:*(l::Quantity, r::Number) = Quantity(l.value * r, l.dimensions)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.value, r.dimensions)
Base.:*(l::Dimensions, r::Number) = Quantity(r, l)
Base.:*(l::Number, r::Dimensions) = Quantity(l, r)

Base.:/(l::Dimensions, r::Dimensions) = @map_dimensions(-, l, r)
Base.:/(l::Quantity, r::Quantity) = Quantity(l.value / r.value, l.dimensions / r.dimensions)
Base.:/(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions / r)
Base.:/(l::Dimensions, r::Quantity) = Quantity(inv(r.value), l / r.dimensions)
Base.:/(l::Quantity, r::Number) = Quantity(l.value / r, l.dimensions)
Base.:/(l::Number, r::Quantity) = l * inv(r)
Base.:/(l::Dimensions, r::Number) = Quantity(inv(r), l)
Base.:/(l::Number, r::Dimensions) = Quantity(l, inv(r))

function Base.:+(l::Quantity, r::Quantity)
    if iszero(l)
        return r
    elseif iszero(r)
        return l
    else
        l.dimensions == r.dimensions ? Quantity(l.value + r.value, l.dimensions) : throw(DimensionError(l, r))
    end
end


function Base.:-(l::Quantity, r::Quantity)
    if iszero(l)
        return -r
    elseif iszero(r)
        return l
    else
        l.dimensions == r.dimensions ?  Quantity(l.value - r.value, l.dimensions) :  throw(DimensionError(l, r))
    end
end

Base.:-(l::Quantity) =  Quantity(- l.value, l.dimensions)

_pow(l::Dimensions{R}, r::R) where {R} = @map_dimensions(Base.Fix1(*, r), l)
_pow(l::Quantity{T,R}, r::R) where {T,R} = Quantity(l.value^convert(T, r), _pow(l.dimensions, r))
Base.:^(l::Dimensions{R}, r::Number) where {R} = _pow(l, tryrationalize(R, r))
Base.:^(l::Quantity{T,R}, r::Number) where {T,R} = _pow(l, tryrationalize(R, r))

Base.inv(d::Dimensions) = @map_dimensions(-, d)
Base.inv(q::Quantity) = Quantity(inv(q.value), inv(q.dimensions))

Base.sqrt(d::Dimensions{R}) where {R} = d^inv(convert(R, 2))
Base.sqrt(q::Quantity) = Quantity(sqrt(q.value), sqrt(q.dimensions))
Base.cbrt(d::Dimensions{R}) where {R} = d^inv(convert(R, 3))
Base.cbrt(q::Quantity) = Quantity(cbrt(q.value), cbrt(q.dimensions))


#
# We need this for pivoting: we could introduce a pivoting type RowNonZero instead.
# 
#Base.abs(q::Quantity) = Quantity(abs(q.value), q.dimensions, q.valid)
Base.abs(q::Quantity) = Quantity(abs(q.value))


Base.iszero(d::Dimensions) = d==Dimensions()
Base.isless(q::Quantity,r::Quantity) =  q.value<r.value && q.dimensions==r.dimensions
Base.isone(q::Quantity{T}) where T =   isone(q.value) && iszero(q.dimensions)
Base.iszero(q::Quantity{T}) where T  =  iszero(q.value)

