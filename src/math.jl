Base.:*(l::Dimensions, r::Dimensions) = Dimensions(+, l, r)
Base.:*(l::Quantity, r::Quantity) = Quantity(l.value * r.value, l.dimensions * r.dimensions, l.valid && r.valid)
Base.:*(l::Quantity, r::Number) = Quantity(l.value * r, l.dimensions, l.valid)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.value, r.dimensions, r.valid)

Base.:/(l::Dimensions, r::Dimensions) = Dimensions(-, l, r)
Base.:/(l::Quantity, r::Quantity) = Quantity(l.value / r.value, l.dimensions / r.dimensions, l.valid && r.valid)
Base.:/(l::Quantity, r::Number) = Quantity(l.value / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity) = l * inv(r)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.value + r.value, l.dimensions, l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.value - r.value, l.dimensions, l.dimensions == r.dimensions)

Base.:^(l::Quantity, r::Quantity) =
    let rr = tryrationalize(Int, r.value)
        Quantity(l.value^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
Base.:^(l::Dimensions, r::Rational{Int}) = Dimensions(Base.Fix1(*, r), l)
Base.:^(l::Quantity, r::Number) =
    let rr = tryrationalize(Int, r)
        Quantity(l.value^rr, l.dimensions^rr, l.valid)
    end

Base.inv(d::Dimensions) = Dimensions(-, d)
Base.inv(q::Quantity) = Quantity(inv(q.value), inv(q.dimensions), q.valid)

Base.sqrt(q::Quantity) = Quantity(sqrt(q.value), q.dimensions^(1 // 2), q.valid)
Base.cbrt(q::Quantity) = Quantity(cbrt(q.value), q.dimensions^(1 // 3), q.valid)