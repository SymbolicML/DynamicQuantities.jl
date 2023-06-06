Base.float(q::Quantity{T}) where {T<:AbstractFloat} = convert(T, q)
Base.convert(::Type{T}, q::Quantity) where {T<:Real} =
    let
        @assert q.valid "Quantity $(q) is invalid!"
        @assert iszero(q.dimensions) "Quantity $(q) has dimensions! Use `ustrip` instead."
        return convert(T, q.value)
    end

Base.isfinite(q::Quantity) = isfinite(q.value)
Base.keys(d::Dimensions) = keys(d.data)
Base.values(d::Dimensions) = values(d.data)
Base.iszero(d::Dimensions) = all(iszero, values(d))
Base.iszero(q::Quantity) = iszero(q.value)
Base.getindex(d::Dimensions, k::Int) = d.data[k]
Base.getindex(d::Dimensions, k::Symbol) = d.data[k]
Base.:(==)(l::Dimensions, r::Dimensions) = all(k -> (l[k] == r[k]), keys(l))
Base.:(==)(l::Quantity, r::Quantity) = l.value == r.value && l.dimensions == r.dimensions && l.valid == r.valid
Base.:(≈)(l::Quantity, r::Quantity) = l.value ≈ r.value && l.dimensions == r.dimensions && l.valid == r.valid
Base.length(::Dimensions) = 1
Base.length(::Quantity) = 1
Base.iterate(d::Dimensions) = (d, nothing)
Base.iterate(::Dimensions, ::Nothing) = nothing
Base.iterate(q::Quantity) = (q, nothing)
Base.iterate(::Quantity, ::Nothing) = nothing
Base.map(f::F, l::Dimensions, r::Dimensions) where {F<:Function} = Dimensions(ntuple(k -> f(l[k], r[k]), Val(NumDimensions)))
Base.map(f::F, l::Dimensions) where {F<:Function} = Dimensions(ntuple(k -> f(l[k]), Val(NumDimensions)))

Base.show(io::IO, d::Dimensions) =
    let tmp_io = IOBuffer()
        foreach(keys(d)) do k
            if !iszero(d[k])
                print(tmp_io, k)
                pretty_print_exponent(tmp_io, d[k])
                print(tmp_io, " ")
            end
        end
        s = String(take!(tmp_io))
        s = replace(s, r"^\s*" => "")
        s = replace(s, r"\s*$" => "")
        print(io, s)
    end
Base.show(io::IO, q::Quantity) = q.valid ? print(io, q.value, " ", q.dimensions) : print(io, "INVALID")

tryround(x::Rational{Int}) = isinteger(x) ? round(Int, x) : x
pretty_print_exponent(io::IO, x::Rational{Int}) =
    let
        if x >= 0 && isinteger(x)
            print(io, "^", round(Int, x))
        else
            print(io, "^(", tryround(x), ")")
        end
    end
@inline tryrationalize(::Type{T}, x::Rational{T}) where {T<:Integer} = x
@inline tryrationalize(::Type{T}, x::T) where {T<:Integer} = Rational{T}(x)
@inline tryrationalize(::Type{T}, x) where {T<:Integer} = rationalize(T, x)

ustrip(q::Quantity) = q.value
ustrip(q::Number) = q
dimensions(q::Quantity) = q.dimensions
dimensions(q::Number) = Dimensions()
valid(q::Quantity) = q.valid

ulength(q::Quantity) = q.dimensions[:𝐋]
umass(q::Quantity) = q.dimensions[:𝐌]
utime(q::Quantity) = q.dimensions[:𝐓]
ucurrent(q::Quantity) = q.dimensions[:𝐈]
utemperature(q::Quantity) = q.dimensions[:𝚯]
uluminosity(q::Quantity) = q.dimensions[:𝐉]
uamount(q::Quantity) = q.dimensions[:𝐍]
