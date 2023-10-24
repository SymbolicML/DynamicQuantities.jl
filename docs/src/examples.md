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

julia> uconvert(us"nm", λ) # return answer in nanometers
3.0294912478780556 nm
```

Since units are automatically propagated, we can verify the dimension of our answer and all intermediates.
Also, using `DynamicQuantities.Constants`, we were able to obtain the (dimensionful!) values of all necessary constants without typing them ourselves.

## 2. Projectile motion

Let's solve a simple projectile motion problem.
First load the `DynamicQuantities` module:

```julia
using DynamicQuantities
```

Set up initial conditions as quantities:

```julia
y0 = 10u"km"
v0 = 250u"m/s"
θ = deg2rad(60)
g = 9.81u"m/s^2"
```

Next, we use trig functions to calculate x and y components of initial velocity.
`vx0` is the x component and
`vy0` is the y component:

```julia
vx0 = v0 * cos(θ)
vy0 = v0 * sin(θ)
```

Next, let's create a time vector from 0 to 1.3 minutes.
Note that these are the same dimension (time), so it's fine to treat
them as dimensionally equivalent!

```julia
t = range(0u"s", 1.3u"min", length=100)
```

Next, use kinematic equations to calculate x and y as a function of time.
`x(t)` is x position at time t, and
`y(t)` is the y position

```julia
x(t) = vx0*t
y(t) = vy0*t - 0.5*g*t^2 + y0
```

These are functions, so let's evaluate them:

```julia
x_si = x.(t)
y_si = y.(t)
```

These are regular vectors of quantities
with `Dimensions` for physical dimensions.

Next, let's plot the trajectory.
First convert to km and strip units:

```julia
x_km = ustrip.(uconvert(us"km").(x_si))
y_km = ustrip.(uconvert(us"km").(y_si))
```

Now, we plot:

```julia
plot(x_km, y_km, label="Trajectory", xlabel="x [km]", ylabel="y [km]")
```

## 3. Various Simple Examples

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
rather than `Number`.

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
y = Quantity(2.0, MyDimensions(cookie=1))

x * y
```

which gives us `3.0 cookie² milk⁻¹`. Likewise,
we can use this in a `QuantityArray`:

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
specialize on `AbstractUnionQuantity` which is
the union of these two abstract quantities, _as well as any other future abstract quantity types_,
such as the planned `AbstractRealQuantity`.

```julia
function my_func(x::AbstractUnionQuantity{T,D}) where {T,D}
    # value has type T and dimensions has type D
    return x / ustrip(x)
end
```
