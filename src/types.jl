const R = Rational{Int}
const DIMENSION_NAMES = (:length, :mass, :time, :current, :temperature, :luminosity, :amount)
const DIMENSION_SYNONYMS = (:𝐋, :𝐌, :𝐓, :𝐈, :𝚯, :𝐉, :𝐍)
const SYNONYM_MAPPING = NamedTuple(DIMENSION_NAMES .=> DIMENSION_SYNONYMS)

"""
    Dimensions

A type representing the dimensions of a quantity, with each
field giving the power of the corresponding dimension. For
example, the dimensions of velocity are `Dimensions(length=1, time=-1)`.
"""
struct Dimensions
    length::R
    mass::R
    time::R
    current::R
    temperature::R
    luminosity::R
    amount::R

    Dimensions(length::R, mass::R, time::R, current::R, temperature::R, luminosity::R, amount::R) =
        new(length, mass, time, current, temperature, luminosity, amount)
    Dimensions(; kws...) = Dimensions(
        tryrationalize(Int, get(kws, :length, 0 // 1)),
        tryrationalize(Int, get(kws, :mass, 0 // 1)),
        tryrationalize(Int, get(kws, :time, 0 // 1)),
        tryrationalize(Int, get(kws, :current, 0 // 1)),
        tryrationalize(Int, get(kws, :temperature, 0 // 1)),
        tryrationalize(Int, get(kws, :luminosity, 0 // 1)),
        tryrationalize(Int, get(kws, :amount, 0 // 1)),
    )
end

"""
    Quantity{T}

Physical quantity with value `value` of type `T` and dimensions `dimensions`.
The `valid` field is used to indicate whether the quantity is valid or not
(e.g., due to dimensional error). For example, the velocity of an object
with mass 1 kg and velocity 2 m/s is `Quantity(2, mass=1, length=1, time=-1)`.
"""
struct Quantity{T}
    value::T
    dimensions::Dimensions
    valid::Bool

    Quantity(x; kws...) = new{typeof(x)}(x, Dimensions(; kws...), true)
    Quantity(x, valid::Bool; kws...) = new{typeof(x)}(x, Dimensions(; kws...), valid)
    Quantity(x, d::Dimensions) = new{typeof(x)}(x, d, true)
    Quantity(x, d::Dimensions, valid::Bool) = new{typeof(x)}(x, d, valid)
end
