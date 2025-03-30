# Toy Examples with Code

## 1. Solving a Chemistry Homework Problem

On your chemistry homework, you are faced with the following problem on the photoelectric effect[^1]:

[^1]: Attribution: [MIT OCW](https://ocw.mit.edu/courses/5-111sc-principles-of-chemical-science-fall-2014/resources/mit5_111f14_lec04soln/)

> In a photoelectric effect experiment, electrons are ejected from a titanium surface (work function ``\Phi = 4.33\mathrm{eV}``) following irradition with UV light.
> The energy of the incident UV light is ``7.2 \cdot 10^{-19} \mathrm{J}`` per photon. Calculate the wavelength of the ejected electrons, in nanometers.

Let's solve this problem with `DynamicQuantities.jl`!

```jldoctest examples
julia> using DynamicQuantities

julia> using DynamicQuantities.Constants: h, m_e

julia> Φ = 4.33u"Constants.eV" # work function
6.93742482522e-19 m² kg s⁻²

julia> E = 7.2e-19u"J" # incident energy
7.2e-19 m² kg s⁻²

julia> p = sqrt(2 * m_e * (E - Φ)) # momentum of ejected electrons
2.1871890716439906e-25 m kg s⁻¹

julia> λ = h / p # wavelength of ejected electrons
3.029491247878056e-9 m

julia> λ |> us"nm" # return answer in nanometers (equivalent to `uconvert(us"nm", λ)`)
3.0294912478780556 nm
```

Since units are automatically propagated, we can verify the dimension of our answer and all intermediates.
Also, using `DynamicQuantities.Constants`, we were able to obtain the (dimensionful!) values of all necessary constants without typing them ourselves.

## 2. Projectile motion

Let's solve a simple projectile motion problem.
First load the `DynamicQuantities` module:

```@example projectile
using DynamicQuantities
```

Set up initial conditions as quantities:

```@example projectile
# Can explicitly import units:
using DynamicQuantities: km, m, s, min

y0 = 10km
v0 = 250m/s
θ = deg2rad(60)
g = 9.81m/s^2
nothing # hide
```

Next, we use trig functions to calculate x and y components of initial velocity.
`vx0` is the x component and
`vy0` is the y component:

```@example projectile
vx0 = v0 * cos(θ)
vy0 = v0 * sin(θ)
nothing # hide
```

Next, let's create a time vector from 0 seconds to 1.3 minutes.
Note that these are the same dimension (time), so it's fine to treat
them as dimensionally equivalent!

```@example projectile
t = range(0s, 1.3min, length=100)
nothing # hide
```

Next, use kinematic equations to calculate x and y as a function of time.
`x(t)` is the x position at time t, and
`y(t)` is the y position:

```@example projectile
x(t) = vx0*t
y(t) = vy0*t - 0.5*g*t^2 + y0
nothing # hide
```

These are functions, so let's evaluate them:

```@example projectile
x_si = x.(t)
y_si = y.(t)
nothing # hide
```

These are regular vectors of quantities
with `Dimensions` for physical dimensions.

Next, let's plot the trajectory.
First convert to km and strip units:

```julia
x_km = ustrip.(x_si .|> us"km")
y_km = ustrip.(y_si .|> us"km")
```

Now, we plot:

```julia
plot(x_km, y_km, label="Trajectory", xlabel="x [km]", ylabel="y [km]")
```

See [Plotting](@ref) for more plotting support with units.

## 3. Using dimensional angles

Say that we wish to track angles as a unit, rather than assume
the SI convention that `1 rad = 1`. We can do this by creating
a new struct to track dimensions:

```julia
using DynamicQuantities

struct AngleDimensions{R} <: AbstractDimensions{R}
    length::R
    mass::R
    time::R
    current::R
    temperature::R
    luminosity::R
    amount::R
    angle::R
end
```

Simply by inheriting from `AbstractDimensions`, we get all the
constructors and operations as defined on `Dimensions`:

```julia
julia> x = Quantity(1.0, AngleDimensions(length=1, angle=-1))
1.0 m angle⁻¹
```

However, perhaps we want to set the default `angle` dimension as
`rad`. We can do this by defining a method for `dimension_name`:

```julia
import DynamicQuantities: DynamicQuantities as DQ

function DQ.dimension_name(::AngleDimensions, k::Symbol)
    default_dimensions = (
        length = "m",
        mass = "kg",
        time = "s",
        current = "A",
        temperature = "K",
        luminosity = "cd",
        amount = "mol",
        angle = "rad",
    )
    return get(default_dimensions, k, string(k))
end
```

This gives us the following behavior:

```julia
julia> x = Quantity(1.0, AngleDimensions(length=1, angle=-1))
1.0 m rad⁻¹
```

Next, say that we are working with existing quantities defined using
standard `Dimensions`. We want to promote these to our new `AngleDimensions` type.

For this, we define two functions: `promote_rule` and a constructor for `AngleDimensions`
from regular `Dimensions`:

```julia
function Base.promote_rule(::Type{AngleDimensions{R1}}, ::Type{Dimensions{R2}}) where {R1,R2}
    return AngleDimensions{promote_type(R1, R2)}
end
function Base.convert(::Type{Quantity{T,AngleDimensions{R}}}, q::Quantity{<:Any,<:Dimensions}) where {T,Din,R}
    val = ustrip(q)
    d = dimension(q)
    return Quantity(
        T(val),
        AngleDimensions{R}(;
            d.length, d.mass, d.time, d.current, d.temperature, d.luminosity, d.amount, angle=zero(R)
        )
    )
end
```

This means that whenever a `Quantity{<:Any,<:Dimensions}`
interacts with a `Quantity{<:Any,<:AngleDimensions}`, the result
will be a `Quantity{<:Any,<:AngleDimensions}`, and we will initialize
the angle dimension to 0. (Code not given for `SymbolicDimensions`; you will
probably want to treat the angles in symbolic units explicitly, so that `us"rad"` is correctly
tracked.)

Let's define a constant for `rad`:

```julia
julia> const rad = Quantity(1.0, AngleDimensions(angle = 1))
1.0 rad
```

and use it in a calculation:

```julia
julia> x = 2rad
2.0 rad

julia> y = 10u"min"
600.0 s

julia> angular_velocity = x / y
0.0033333333333333335 s⁻¹ rad
```

which as we can see, automatically promotes to
`AngleDimensions`.

**However, note the following:**
If existing code uses `rad` as a unit without tracking
it with `AngleDimensions`, you will need to explicitly add
the missing dimensions.
For this reason, if you decide to take this approach to tracking units,
you probably want to use `AngleDimensions` throughout your codebase,
rather than mixing them.

## 4. Assorted examples

This section demonstrates miscellaneous examples of using `DynamicQuantities.jl`.

### Conversion

Convert a quantity to have a new type for the value:

```julia
quantity = 1.5u"m"

convert_q = Quantity{Float32}(quantity)

println("Converted Quantity to Float32: ", convert_q)
```

### Array basics

Create a `QuantityArray` (an array of quantities with
the same dimension) by passing an array and a single quantity:

```julia
x = QuantityArray(randn(32), u"km/s")
```

or, by passing an array of individual quantities:

```julia
y = randn(32)
y_q = QuantityArray(y .* u"m * cd / s")
```

We can take advantage of this being `<:AbstractArray`:

```julia
println("Sum x: ", sum(x))
```

We can also do things like setting a particular element:

```julia
y_q[5] = Quantity(5, length=1, luminosity=1, time=-1)
println("5th element of y_q: ", y_q[5])
```

We can get back the original array with `ustrip`:

```julia
println("Stripped y_q: ", ustrip(y_q))
```

This `QuantityArray` is useful for broadcasting:

```julia
f_square(v) = v^2 * 1.5 - v^2
println("Applying function to y_q: ", sum(f_square.(y_q)))
```

### Fill

We can also make `QuantityArray` using `fill`:

```julia
filled_q = fill(u"m/s", 10)
println("Filled QuantityArray: ", filled_q)
```

`fill` works for 0 dimensional `QuantityArray`s as well:

```julia
empty_q = fill(u"m/s", ())
println("0 dimensional QuantityArray: ", empty_q)
```

### Similar

Likewise, we can create a `QuantityArray` with the same properties as another `QuantityArray`:

```julia
qa = QuantityArray(rand(3, 4), u"m")

new_qa = similar(qa)

println("Similar qa: ", new_qa)
```

### Promotion

Promotion rules are defined for `QuantityArray`s:

```julia
qarr1 = QuantityArray(randn(32), convert(Dimensions{Rational{Int32}}, dimension(u"km/s")))
qarr2 = QuantityArray(randn(Float16, 32), convert(Dimensions{Rational{Int64}}, dimension(u"km/s")))
```

See what type they promote to:

```julia
println("Promoted type: ", typeof(promote(qarr1, qarr2)))
```

### Array Concatenation

Likewise, we can take advantage of array concatenation,
which will ensure we have the same dimensions:

```julia
qarr1 = QuantityArray(randn(3) .* u"km/s")
qarr2 = QuantityArray(randn(3) .* u"km/s")
```

Concatenate them:

```julia
concat_qarr = hcat(qarr1, qarr2)
println("Concatenated QuantityArray: ", concat_qarr)
```

### Symbolic Units

We can use arbitrary `AbstractQuantity` and `AbstractDimensions`
in a `QuantityArray`, including `SymbolicDimensions`:

```julia
z_ar = randn(32)
z = QuantityArray(z_ar, us"Constants.M_sun * km/s")
```

Expand to standard units:

```julia
z_expanded = uexpand(z)
println("Expanded z: ", z_expanded)
```


### GenericQuantity Construction

In addition to `Quantity`, we can also use `GenericQuantity`:


```julia
x = GenericQuantity(1.5)
y = GenericQuantity(0.2u"km")
println(y)
```

This `GenericQuantity` is subtyped to `Any`,
rather than `Number`, and thus can also store
custom non-scalar types.

For example, we can work with `Coords`, and
wrap it in a single `GenericQuantity` type:

```julia
struct Coords
    x::Float64
    y::Float64
end

# Define arithmetic operations on Coords
Base.:+(a::Coords, b::Coords) = Coords(a.x + b.x, a.y + b.y)
Base.:-(a::Coords, b::Coords) = Coords(a.x - b.x, a.y - b.y)
Base.:*(a::Coords, b::Number) = Coords(a.x * b, a.y * b)
Base.:*(a::Number, b::Coords) = Coords(a * b.x, a * b.y)
Base.:/(a::Coords, b::Number) = Coords(a.x / b, a.y / b)
```

We can then build a `GenericQuantity` out of this:

```julia
coord1 = GenericQuantity(Coords(0.3, 0.9), length=1)
coord2 = GenericQuantity(Coords(0.2, -0.1), length=1)
```

and perform operations on these:

```julia
coord1 + coord2 |> us"cm"
# (Coords(50.0, 80.0)) cm
```

The nice part about this is it only stores a single Dimensions
(or `SymbolicDimensions`) for the entire struct!

### GenericQuantity and Quantity Promotion

When we combine a `GenericQuantity` and a `Quantity`,
the result is another `GenericQuantity`:

```julia
x = GenericQuantity(1.5f0)
y = Quantity(1.5, length=1)
println("Promoted type of x and y: ", typeof(x * y))
```

### Custom Dimensions

We can create custom dimensions by subtyping to
`AbstractDimensions`:

```julia
struct MyDimensions{R} <: AbstractDimensions{R}
    cookie::R
    milk::R
end
```

Many constructors and functions are defined on `AbstractDimensions`,
so this can be used out-of-the-box.
We can then use this in a `Quantity`, and all operations will work as expected:

```julia
x = Quantity(1.5, MyDimensions(cookie=1, milk=-1))
y = Quantity(2.0, MyDimensions(milk=1))

x * y
```

which gives us `3.0 cookie` computed from a rate of `1.5 cookie milk⁻¹` multiplied
by `2.0 milk`. Likewise, we can use these in a `QuantityArray`:

```julia
x_qa = QuantityArray(randn(32), MyDimensions(cookie=1, milk=-1))

x_qa .^ 2
```

### Custom Quantities

We can also create custom dimensions by subtyping
to either `AbstractQuantity` (for `<:Number`) or
`AbstractGenericQuantity` (for `<:Any`):

```julia
struct MyQuantity{T,D} <: AbstractQuantity{T,D}
    value::T
    dimensions::D
end
```

Since `AbstractQuantity <: Number`, this will also be a number.
Keep in mind that you must call these fields `value` and `dimensions`
for `ustrip(...)` and `dimension(...)` to work. Otherwise, simply
redefine those.

We can use this custom quantity just like we would use `Quantity`:

```julia
q1 = MyQuantity(1.2, Dimensions(length=-2))
# prints as `1.2 m⁻²`

q2 = MyQuantity(1.5, MyDimensions(cookie=1))
# prints as `1.5 cookie`
```

Including mathematical operations:

```julia
q2 ^ 2
# `2.25 cookie²`
```

The main reason you would use a custom quantity is if you want
to change built-in behavior, or maybe have special methods for
different types of quantities.

Note that you can declare a method on `AbstractQuantity`, or
`AbstractGenericQuantity` to allow their respective inputs.

**Note**: In general, you should probably
specialize on `UnionAbstractQuantity` which is
the union of these two abstract quantities, _as well as any other future abstract quantity types_,
such as the planned `AbstractRealQuantity`.

```julia
function my_func(x::UnionAbstractQuantity{T,D}) where {T,D}
    # value has type T and dimensions has type D
    return x / ustrip(x)
end
```


### Plotting

We can use [`Makie.jl`](https://docs.makie.org/v0.22/) to create plots with units. Below are a few usage examples. See [Makie.jl > Dimension conversion](https://docs.makie.org/stable/explanations/dim-converts#Current-conversions-in-Makie) for more.

!!! warning "Experimental"
    Unit support is still a new feature, so please report an issue if you notice any unintended behavior.


Continuing from [2. Projectile motion](@ref), we can also plot `x_si` and `y_si` directly without needing to manually strip their units beforehand:

```@example projectile
using CairoMakie

lines(x_si, y_si; axis=(xlabel="x", ylabel="y"))
```

To convert units, we pass a `DQConversion` object to `axis` with our desired unit:

```@example projectile
# Temporary until this is upstreamed to Makie.jl
const DQConversion = Base.get_extension(DynamicQuantities, :DynamicQuantitiesMakieExt).DQConversion

lines(x_si, y_si;
    axis = (
        xlabel = "x",
        ylabel = "y",
        dim1_conversion = DQConversion(us"km"),
        dim2_conversion = DQConversion(us"km"),
    )
)
```

!!! warning
    Make sure to use [Symbolic Dimensions](@ref) for this conversion to work properly.

Finally, the desired units for a figure can also be set ahead of time. All plot objects within it will automatically convert to the given units:

```@example projectile
fig = Figure()

ax = Axis(fig[1, 1];
    xlabel = "time",
    ylabel = "displacement",
    dim1_conversion=DQConversion(us"s"),
    dim2_conversion=DQConversion(us"km"),
)

lines!(ax, t, x_si; label="x")
lines!(ax, t, y_si; label="y")

axislegend()

fig
```
