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


## 2. Various simple examples

Here, let's look at various things we can do with DynamicQuantities.jl.

### Projectile motion

```julia
using DynamicQuantities
```

First, the initial conditions:

```julia
y0 = 10u"km"        # Initial position
v0 = 250u"m/s"      # Initial velocity
θ = deg2rad(60)     # Launch angle
g = 9.81u"m/s^2"    # Acceleration due to gravity
```

Let's calculate components of initial velocity:

```julia
vx0 = v0 * cos(θ)
vy0 = v0 * sin(θ)
```

Let's simply plug it into the equations of motion,
from 0 seconds to 1.3 minutes:

```julia
t = range(0u"s", 1.3u"min", length=100)

x(t) = vx0*t
y(t) = vy0*t - 0.5*g*t^2 + y0

# Compute trajectory:
x_si = x.(t)
y_si = y.(t)
```

Now, let's plot it:

```julia
# Plot
using Plots

# Convert to km and strip:
x_km = x_si .|> uconvert(us"km") .|> ustrip
y_km = y_si .|> uconvert(us"km") .|> ustrip

plot(x_km, y_km, label="Trajectory", xlabel="x [km]", ylabel="y [km]")
```

### Conversion

```julia
quantity = 1.5u"m"
println("Converted Quantity to Float32: ", Quantity{Float32}(quantity))
```

### Arrays Basics

```julia
using DynamicQuantities
x = QuantityArray(randn(32), u"km/s")

y = randn(32)

y_array_of_q = y .* u"m * cd / s"
y_q = QuantityArray(y_array_of_q)

# Summing QuantityArray
println("Sum x: ", sum(x))

# Setting index with different quantity
x[5] = Quantity(5, length=1, time=-1)
println("5th element of x: ", x[5])

# Checking if it strips to original values
println("Stripped y_q equals to y: ", ustrip(QuantityArray(y, u"m")) == y)

# Applying a function to QuantityArray
f_square(v) = v^2 * 1.5 - v^2
println("Applying function to y_q: ", sum(f_square.(y_q)))
```

### Utilities

```julia
# Using fill function to create a QuantityArray
println("Filled QuantityArray: ", fill(u"m/s", 10))

# Check if fill function can create 0 dimensional QuantityArray
println("0 dimensional QuantityArray: ", fill(u"m/s", ()))
```

### Similar

```julia
qa = QuantityArray(rand(3, 4), u"m")

# Creating a similar QuantityArray
new_qa = similar(qa)
println("Similar qa: ", new_qa)
```

### Promotion

```julia
qarr1 = QuantityArray(randn(32), convert(Dimensions{Rational{Int32}}, dimension(u"km/s")))
qarr2 = QuantityArray(randn(Float16, 32), convert(Dimensions{Rational{Int64}}, dimension(u"km/s")))

# Checking the promotion rules between QuantityArrays
println("Promotion rules: ", typeof(promote(qarr1, qarr2)))
```

### Array concatenation

```julia
qarr1 = QuantityArray(randn(3) .* u"km/s")
qarr2 = QuantityArray(randn(3) .* u"km/s")

# Concatenating QuantityArrays
println("Concatenated QuantityArray: ", hcat(qarr1, qarr2))
```

### Broadcasted power operation

```julia
y_q = QuantityArray(randn(32), u"m")

# Applying a function with power operation to QuantityArray
f4(v) = v^4 * 0.3
println("Power operation to y_q: ", sum(f4, y_q))
```

### Broadcasting nd-arrays

```julia
# Broadcasting operation between two 2D QuantityArrays
x = QuantityArray(randn(3, 3), u"A")
y = QuantityArray(randn(3, 3), u"cd")
println("Broadcasted QuantityArray: ", x .* y)
```

### Symbolic units

```julia
# Creating QuantityArray with symbolic units
z_ar = randn(32)
z = QuantityArray(z_ar, us"Constants.h * km/s")
println("Expanded z: ", uexpand(z))
```

### GenericQuantity construction

```julia
x = GenericQuantity(1.5)
println("Generic Quantity: ", x)
```

This `GenericQuantity` is subtyped to `Any`,
rather than `Number`.

### GenericQuantity and Quantity promotion

When we combine a `GenericQuantity` and a `Quantity`,
the result is another `GenericQuantity`:

```julia
x = GenericQuantity(1.5f0)
y = Quantity(1.5, length=1)
println("Promoted type of x and y: ", typeof(x * y))
```
