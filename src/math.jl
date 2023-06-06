Base.:*(l::Dimensions, r::Dimensions) = Dimensions((x, y) -> x + y, l, r)
Base.:*(l::Quantity, r::Quantity) = Quantity(l.val * r.val, l.dimensions * r.dimensions, l.valid && r.valid)
Base.:*(l::Quantity, r::Number) = Quantity(l.val * r, l.dimensions, l.valid)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.val, r.dimensions, r.valid)

Base.:/(l::Dimensions, r::Dimensions) = Dimensions((x, y) -> x - y, l, r)
Base.:/(l::Quantity, r::Quantity) = Quantity(l.val / r.val, l.dimensions / r.dimensions, l.valid && r.valid)
Base.:/(l::Quantity, r::Number) = Quantity(l.val / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity) = l * inv(r)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.val + r.val, l.dimensions, l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.val - r.val, l.dimensions, l.dimensions == r.dimensions)

Base.:^(l::Quantity, r::Quantity) =
    let rr = tryrationalize(Int, r.val)
        Quantity(l.val^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
Base.:^(l::Dimensions, r::Rational{Int}) = Dimensions(x -> x * r, l)
Base.:^(l::Quantity, r::Number) =
    let rr = tryrationalize(Int, r)
        Quantity(l.val^rr, l.dimensions^rr, l.valid)
    end

Base.inv(d::Dimensions) = Dimensions((-d[k] for k in keys(d)))
Base.inv(q::Quantity) = Quantity(inv(q.val), inv(q.dimensions), q.valid)

Base.sqrt(q::Quantity) = Quantity(sqrt(q.val), q.dimensions^(1 // 2), q.valid)
Base.cbrt(q::Quantity) = Quantity(cbrt(q.val), q.dimensions^(1 // 3), q.valid)