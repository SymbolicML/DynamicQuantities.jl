Base.:*(l::Dimensions, r::Dimensions) = Dimensions(D_TYPE([(k, l[k] + r[k]) for k in union(keys(l.data), keys(r.data))]))
Base.:*(l::Quantity, r::Quantity) = Quantity(l.val * r.val, l.dimensions * r.dimensions, l.valid && r.valid)
Base.:*(l::Quantity, r::Number) = Quantity(l.val * r, l.dimensions, l.valid)
Base.:*(l::Number, r::Quantity) = Quantity(l * r.val, r.dimensions, r.valid)

Base.:/(l::Dimensions, r::Dimensions) = Dimensions(D_TYPE([(k, l[k] - r[k]) for k in union(keys(l.data), keys(r.data))]))
Base.:/(l::Quantity, r::Quantity) = Quantity(l.val / r.val, l.dimensions / r.dimensions, l.valid && r.valid)
Base.:/(l::Quantity, r::Number) = Quantity(l.val / r, l.dimensions, l.valid)
Base.:/(l::Number, r::Quantity) = l * inv(r)

Base.:+(l::Quantity, r::Quantity) = Quantity(l.val + r.val, l.dimensions, l.dimensions == r.dimensions)
Base.:-(l::Quantity, r::Quantity) = Quantity(l.val - r.val, l.dimensions, l.dimensions == r.dimensions)

Base.inv(d::Dimensions) = Dimensions(D_TYPE([(k, -d[k]) for k in keys(d.data)]))
Base.inv(q::Quantity) = Quantity(inv(q.val), inv(q.dimensions), q.valid)

Base.sqrt(q::Quantity) = Quantity(sqrt(q.val), q.dimensions^(1 // 2), q.valid)
Base.cbrt(q::Quantity) = Quantity(cbrt(q.val), q.dimensions^(1 // 3), q.valid)

Base.:^(l::Quantity, r::Quantity) =
    let rr = convert(Rational{Int}, r.val)
        Quantity(l.val^rr, l.dimensions^rr, l.valid && r.valid && iszero(r.dimensions))
    end
Base.:^(l::Dimensions, r::Rational{Int}) = Dimensions(D_TYPE([(k, l.data[k] * r) for k in keys(l.data)]))
Base.:^(l::Quantity, r::Number) =
    let rr = convert(Rational{Int}, r)
        Quantity(l.val^rr, l.dimensions^rr, l.valid)
    end