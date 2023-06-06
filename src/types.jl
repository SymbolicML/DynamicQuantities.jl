const D_TYPE = Dict{Symbol,Rational{Int}}
const COMPAT_D_TYPE = Union{D_TYPE,Vector{Pair{Symbol,Rational{Int}}},Vector{Pair{Symbol,Int}}}

struct Dimensions
    data::D_TYPE

    Dimensions() = new(D_TYPE())
    Dimensions(data::D_TYPE) = new(data)
    Dimensions(data::COMPAT_D_TYPE) = new(D_TYPE(data))
end
struct Quantity{T}
    val::T
    dimensions::Dimensions
    valid::Bool

    Quantity(x) = new{typeof(x)}(x, Dimensions(), true)
    Quantity(x, dimensions::Dimensions) = new{typeof(x)}(x, dimensions, true)
    Quantity(x, data::COMPAT_D_TYPE) = new{typeof(x)}(x, Dimensions(data), true)
    Quantity(x, valid::Bool) = new{typeof(x)}(x, Dimensions(), valid)
    Quantity(x, dimensions::Dimensions, valid::Bool) = new{typeof(x)}(x, dimensions, valid)
    Quantity(x, data::COMPAT_D_TYPE, valid::Bool) = new{typeof(x)}(x, Dimensions(data), valid)
end