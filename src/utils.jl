Base.float(q::Quantity{T}) where {T<:AbstractFloat} = convert(T, q)
Base.convert(::Type{T}, q::Quantity) where {T<:Real} =
    let
        @assert q.valid "Quantity $(q) is invalid!"
        @assert iszero(q.dimensions) "Quantity $(q) has dimensions!"
        return convert(T, q.val)
    end

Base.isfinite(q::Quantity) = isfinite(q.val)
Base.keys(d::Dimensions) = keys(d.data)
Base.values(d::Dimensions) = values(d.data)
Base.iszero(d::Dimensions) = all(iszero, values(d))
Base.getindex(d::Dimensions, k::Symbol) = d.data[k]
Base.:(==)(l::D, r::D) where {D<:Dimensions} = all(k -> (l[k] == r[k]), keys(l))
Base.:(==)(l::Q, r::Q) where {Q<:Quantity} = l.val == r.val && l.dimensions == r.dimensions && l.valid == r.valid

Base.show(io::IO, d::Dimensions) =
    foreach(keys(d)) do k
        if !iszero(d[k])
            print(io, k)
            pretty_print_exponent(io, d[k])
            print(io, " ")
        end
    end
Base.show(io::IO, q::Quantity) = q.valid ? print(io, q.val, " ", q.dimensions) : print(io, "INVALID")
tryround(x::Rational{Int}) = isinteger(x) ? round(Int, x) : x
pretty_print_exponent(io::IO, x::Rational{Int}) =
    let
        if x >= 0 && isinteger(x)
            print(io, "^", round(Int, x))
        else
            print(io, "^(", tryround(x), ")")
        end
    end