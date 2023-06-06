const R = Rational{Int}
const DIMENSION_NAMES = (:length, :mass, :time, :current, :temperature, :luminosity, :amount)
const DIMENSION_SYNONYMS = (:𝐋, :𝐌, :𝐓, :𝐈, :𝚯, :𝐉, :𝐍)
const SYNONYM_MAPPING = NamedTuple(DIMENSION_NAMES .=> DIMENSION_SYNONYMS)

Base.@kwdef struct Dimensions
    length::R = 0 // 1
    mass::R = 0 // 1
    time::R = 0 // 1
    current::R = 0 // 1
    temperature::R = 0 // 1
    luminosity::R = 0 // 1
    amount::R = 0 // 1
end

struct Quantity{T}
    value::T
    dimensions::Dimensions
    valid::Bool

    Quantity(x; kws...) = new{typeof(x)}(x, Dimensions(; kws...), true)
    Quantity(x, valid::Bool; kws...) = new{typeof(x)}(x, Dimensions(; kws...), valid)
    Quantity(x, d::Dimensions) = new{typeof(x)}(x, d, true)
    Quantity(x, d::Dimensions, valid::Bool) = new{typeof(x)}(x, d, valid)
end
