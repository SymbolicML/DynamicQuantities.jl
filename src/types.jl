import Ratios: SimpleRatio
import SaferIntegers: SafeInt

"""
    Dimensions

A type representing the dimensions of a quantity, with each
field giving the power of the corresponding dimension. For
example, the dimensions of velocity are `Dimensions(length=1, time=-1)`.

# Fields

- `length`: length dimension (i.e., meters^(length))
- `mass`: mass dimension (i.e., kg^(mass))
- `time`: time dimension (i.e., s^(time))
- `current`: current dimension (i.e., A^(current))
- `temperature`: temperature dimension (i.e., K^(temperature))
- `luminosity`: luminosity dimension (i.e., cd^(luminosity))
- `amount`: amount dimension (i.e., mol^(amount))
"""
struct Dimensions{R <: Real}
    length::R
    mass::R
    time::R
    current::R
    temperature::R
    luminosity::R
    amount::R

    function Dimensions(length::R,
                        mass::R,
                        time::R,
                        current::R,
                        temperature::R,
                        luminosity::R,
                        amount::R) where R
        new{R}(length, mass, time, current, temperature, luminosity, amount)
    end
    Dimensions(; kws...) = Dimensions(SimpleRatio{SafeInt}; kws...)
    Dimensions(R; kws...) = Dimensions(
        tryrationalize(R, get(kws, :length,      zero(R))),
        tryrationalize(R, get(kws, :mass,        zero(R))),
        tryrationalize(R, get(kws, :time,        zero(R))),
        tryrationalize(R, get(kws, :current,     zero(R))),
        tryrationalize(R, get(kws, :temperature, zero(R))),
        tryrationalize(R, get(kws, :luminosity,  zero(R))),
        tryrationalize(R, get(kws, :amount,      zero(R))),
    )
end

const DIMENSION_NAMES = Base.fieldnames(Dimensions)
const DIMENSION_SYNONYMS = (:𝐋, :𝐌, :𝐓, :𝐈, :𝚯, :𝐉, :𝐍)
const SYNONYM_MAPPING = NamedTuple(DIMENSION_NAMES .=> DIMENSION_SYNONYMS)

"""
    Quantity{T}

Physical quantity with value `value` of type `T` and dimensions `dimensions`.
The `valid` field is used to indicate whether the quantity is valid or not
(e.g., due to dimensional error). For example, the velocity of an object
with mass 1 kg and velocity 2 m/s is `Quantity(2, mass=1, length=1, time=-1)`.
You should access these fields with `ustrip(q)`, `dimensions(q)`, and `valid(q)`.
You can access specific dimensions with `ulength(q)`, `umass(q)`, `utime(q)`,
`ucurrent(q)`, `utemperature(q)`, `uluminosity(q)`, and `uamount(q)`.

Severals operators in `Base` are extended to work with `Quantity` objects,
including `*`, `+`, `-`, `/`, `^`, `sqrt`, and `cbrt`.

# Fields

- `value::T`: value of the quantity of some type `T`
- `dimensions::Dimensions`: dimensions of the quantity
- `valid::Bool`: whether the quantity is valid or not
"""
struct Quantity{T, R}
    value::T
    dimensions::Dimensions{R}
    valid::Bool

    Quantity(x; kws...) = new{typeof(x), SimpleRatio{SafeInt}}(x, Dimensions(; kws...), true)
    Quantity(x, valid::Bool; kws...) = new{typeof(x), SimpleRatio{SafeInt}}(x, Dimensions(; kws...), valid)
    Quantity(x, valid::Bool, ::Type{R}; kws...) where {R} = new{typeof(x), R}(x, Dimensions(R; kws...), valid)
    Quantity(x, ::Type{R}; kws...) where {R}  = new{typeof(x), R}(x, Dimensions(R; kws...), true)
    Quantity(x, d::Dimensions{R}) where {R}  = new{typeof(x), R}(x, d, true)
    Quantity(x, d::Dimensions{R}, valid::Bool) where {R}  = new{typeof(x), R}(x, d, valid)
end
