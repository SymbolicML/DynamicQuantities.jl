Base.:*(l::D, r::D) where {D<:Dimensions} = @map_dimensions(D, +, l, r)
Base.:*(l::Quantity, r::Quantity) = Quantity(l.value * r.value, l.dimensions * r.dimensions, l.valid && r.valid)
Base.:*(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions * r, l.valid)
Base.:*(l::Dimensions, r::Quantity) = Quantity(r.value, l * r.dimensions, r.valid)
Base.:*(l::Quantity, r::Number) = Quantity(l.value * r, l.dimensions, l.valid)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.value, r.dimensions, r.valid)
Base.:*(l::Dimensions, r::Number) = Quantity(r, l, true)
Base.:*(l::Number, r::Dimensions) = Quantity(l, r, true)

Base.:/(l::D, r::D) where {D<:Dimensions} = @map_dimensions(D, -, l, r)
Base.:/(l::Quantity, r::Quantity) = Quantity(l.value / r.value, l.dimensions / r.dimensions, l.valid && r.valid)
Base.:/(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions / r, l.valid)
Base.:/(l::Dimensions, r::Quantity) = Quantity(inv(r.value), l / r.dimensions, r.valid)
Base.:/(l::Quantity, r::Number) = Quantity(l.value / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity) = l * inv(r)
Base.:/(l::Dimensions, r::Number) = Quantity(inv(r), l, true)
Base.:/(l::Number, r::Dimensions) = Quantity(l, inv(r), true)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.value + r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.value - r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)

Base.:^(l::Quantity{T,R}, r::Quantity{T,R}) where {T,R} =
    let rr = tryrationalize(R, r.value)
        Quantity(l.value^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
Base.:^(l::Dimensions{R}, r::R) where {R} = @map_dimensions(typeof(l), Base.Fix1(*, r), l)
Base.:^(l::Dimensions{R}, r::Number) where {R} = l^tryrationalize(R, r)
Base.:^(l::Quantity{T,R}, r::Number) where {T,R} =
    let rr = tryrationalize(R, r)
        Quantity(l.value^rr, l.dimensions^rr, l.valid)
    end

Base.inv(d::Dimensions) = @map_dimensions(typeof(d), -, d)
Base.inv(q::Quantity) = Quantity(inv(q.value), inv(q.dimensions), q.valid)

Base.sqrt(d::Dimensions{R}) where {R} = d^R(1 // 2)
Base.sqrt(q::Quantity) = Quantity(sqrt(q.value), sqrt(q.dimensions), q.valid)
Base.cbrt(d::Dimensions) = d^R(1 // 3)
Base.cbrt(q::Quantity) = Quantity(cbrt(q.value), cbrt(q.dimensions), q.valid)

Base.abs(q::Quantity) = Quantity(abs(q.value), q.dimensions, q.valid)
