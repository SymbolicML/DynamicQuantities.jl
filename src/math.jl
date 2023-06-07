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
Base.:/(l::Quantity, r::Dimensions) = Quantity(l.value, l.dimensions / r, l.valid)
Base.:/(l::Dimensions, r::Quantity) = Quantity(inv(r.value), l / r.dimensions, r.valid)
Base.:/(l::Quantity, r::Number) = Quantity(l.value / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity) = l * inv(r)
Base.:/(l::Dimensions, r::Number) = Quantity(inv(r), l, true)
Base.:/(l::Number, r::Dimensions) = Quantity(l, inv(r), true)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.value + r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.value - r.value, l.dimensions, l.valid && r.valid && l.dimensions == r.dimensions)

Base.:^(l::Quantity, r::Quantity) =
    let rr = tryrationalize(INT_TYPE, r.value)
        Quantity(l.value^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
Base.:^(l::Dimensions, r::R) = @map_dimensions(Base.Fix1(*, r), l)
Base.:^(l::Dimensions, r::Number) = l^tryrationalize(INT_TYPE, r)
Base.:^(l::Quantity, r::Number) =
    let rr = tryrationalize(INT_TYPE, r)
        Quantity(l.value^rr, l.dimensions^rr, l.valid)
    end

Base.inv(d::Dimensions) = @map_dimensions(-, d)
Base.inv(q::Quantity) = Quantity(inv(q.value), inv(q.dimensions), q.valid)

Base.sqrt(d::Dimensions) = d^(1 // 2)
Base.sqrt(q::Quantity) = Quantity(sqrt(q.value), sqrt(q.dimensions), q.valid)
Base.cbrt(d::Dimensions) = d^(1 // 3)
Base.cbrt(q::Quantity) = Quantity(cbrt(q.value), cbrt(q.dimensions), q.valid)

Base.abs(q::Quantity) = Quantity(abs(q.value), q.dimensions, q.valid)
