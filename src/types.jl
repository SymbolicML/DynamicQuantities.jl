import Ratios: SimpleRatio
import SaferIntegers: SafeInt8

const INT_TYPE = SafeInt8
const R = SimpleRatio{INT_TYPE}
const ZERO = R(0)
const DIMENSION_NAMES = (:length, :mass, :time, :current, :temperature, :luminosity, :amount)
const DIMENSION_SYNONYMS = (:𝐋, :𝐌, :𝐓, :𝐈, :𝚯, :𝐉, :𝐍)
const SYNONYM_MAPPING = NamedTuple(DIMENSION_NAMES .=> DIMENSION_SYNONYMS)

"""
    Dimensions

A type representing the dimensions of a quantity, with each
field giving the power of the corresponding dimension. For
example, the dimensions of velocity are `Dimensions(length=1, time=-1)`.

# Fields

- `length::Rational{Int}`: length dimension (i.e., meters^(length))
- `mass::Rational{Int}`: mass dimension (i.e., kg^(mass))
- `time::Rational{Int}`: time dimension (i.e., s^(time))
- `current::Rational{Int}`: current dimension (i.e., A^(current))
- `temperature::Rational{Int}`: temperature dimension (i.e., K^(temperature))
- `luminosity::Rational{Int}`: luminosity dimension (i.e., cd^(luminosity))
- `amount::Rational{Int}`: amount dimension (i.e., mol^(amount))
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
        tryrationalize(R, get(kws, :length, ZERO)),
        tryrationalize(R, get(kws, :mass, ZERO)),
        tryrationalize(R, get(kws, :time, ZERO)),
        tryrationalize(R, get(kws, :current, ZERO)),
        tryrationalize(R, get(kws, :temperature, ZERO)),
        tryrationalize(R, get(kws, :luminosity, ZERO)),
        tryrationalize(R, get(kws, :amount, ZERO)),
    )
end

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
struct Quantity{T}
    value::T
    dimensions::Dimensions
    valid::Bool

    Quantity(x; kws...) = new{typeof(x)}(x, Dimensions(; kws...), true)
    Quantity(x, valid::Bool; kws...) = new{typeof(x)}(x, Dimensions(; kws...), valid)
    Quantity(x, d::Dimensions) = new{typeof(x)}(x, d, true)
    Quantity(x, d::Dimensions, valid::Bool) = new{typeof(x)}(x, d, valid)
end
