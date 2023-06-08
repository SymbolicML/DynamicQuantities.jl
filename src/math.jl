Base.:*(l::Dimensions, r::Dimensions) = @map_dimensions(+, l, r)
Base.:*(l::Quantity, r::Quantity) = Quantity(l.value * r.value, l.dimensions * r.dimensions, l.valid && r.valid)
Base.:*(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions * r, l.valid)
Base.:*(l::Dimensions, r::Quantity) = Quantity(r.value, l * r.dimensions, r.valid)
Base.:*(l::Quantity, r::Number) = Quantity(l.value * r, l.dimensions, l.valid)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.value, r.dimensions, r.valid)
Base.:*(l::Dimensions, r::Number) = Quantity(r, l, true)
Base.:*(l::Number, r::Dimensions) = Quantity(l, r, true)

Base.:/(l::Dimensions, r::Dimensions) = @map_dimensions(-, l, r)
Base.:/(l::Quantity, r::Quantity) = Quantity(l.value / r.value, l.dimensions / r.dimensions, l.valid && r.valid)
Base.:/(l::Quantity{T,R}, r::Dimensions{R}) where {T,R} = Quantity(l.value, l.dimensions / r, l.valid)
Base.:/(l::Dimensions{R}, r::Quantity{T,R}) where {T,R} = Quantity(inv(r.value), l / r.dimensions, r.valid)
Base.:/(l::Quantity{T}, r::Number) where {T} = Quantity(l.value / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity{T}) where {T} = l * inv(r)
Base.:/(l::Dimensions, r::Number) = Quantity(inv(r), l, true)
Base.:/(l::Number, r::Dimensions) = Quantity(l, inv(r), true)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.value + r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.value - r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)

Base.:^(l::Q, r::Quantity) where {T,R,Q<:Quantity{T,R}} =
    let rr = tryrationalize(R, r.value)
        Quantity(l.value^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
_pow(l::Dimensions{R}, r::R) where {R} = @map_dimensions(Base.Fix1(*, r), l)
Base.:^(l::Dimensions{R}, r::Number) where {R} = _pow(l, tryrationalize(R, r))
Base.:^(l::Quantity{T,R}, r::Number) where {T,R} =
    let rr = tryrationalize(R, r)
        Quantity(l.value^convert(T, rr), l.dimensions^rr, l.valid)
    end

Base.inv(d::Dimensions) = @map_dimensions(-, d)
Base.inv(q::Quantity) = Quantity(inv(q.value), inv(q.dimensions), q.valid)

Base.sqrt(d::Dimensions{R}) where {R} = d^inv(convert(R, 2))
Base.sqrt(q::Quantity) = Quantity(sqrt(q.value), sqrt(q.dimensions), q.valid)
Base.cbrt(d::Dimensions{R}) where {R} = d^inv(convert(R, 3))
Base.cbrt(q::Quantity) = Quantity(cbrt(q.value), cbrt(q.dimensions), q.valid)

Base.abs(q::Quantity) = Quantity(abs(q.value), q.dimensions, q.valid)
