# DynamicUnits

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://symbolicml.org/DynamicUnits.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicUnits.jl/dev/)
[![Build Status](https://github.com/SymbolicML/DynamicUnits.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/SymbolicML/DynamicUnits.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://coveralls.io/repos/github/SymbolicML/DynamicUnits.jl/badge.svg?branch=main)](https://coveralls.io/github/SymbolicML/DynamicUnits.jl?branch=main)

This defines a simple statically-typed `Quantity` type for Julia.
Physical dimensions are stored as a *value*, as opposed to a parametric type, as in [Unitful.jl](https://github.com/PainterQubits/Unitful.jl).
This is done to allow for calculations where physical dimensions are not known at compile time.

## Performance

The performance of DynamicUnits is slower than Unitful if the dimensions are known at compile time:

```julia
julia> using BenchmarkTools

julia> dyn_uni = Quantity(0.2, mass=1, length=0.5, amount=3)

julia> unitful = convert(Unitful.Quantity, dyn_uni)

julia> f(x) = x ^ 2 * 0.3

julia> @btime f($dyn_uni);
  56.317 ns (0 allocations: 0 bytes)

julia> @btime f($unitful);
  1.958 ns (0 allocations: 0 bytes)
```
While both of these are type stable,
because Unitful parametrizes the type on the dimensions, functions can specialize
to units and the compiler can optimize away units from the code.

However, if the dimension is unknown, the performance can suffer quite a bit.
This is where DynamicUnits shines:

```julia
julia> g(x) = x ^ rand(1:10) * 0.3;

julia> @btime g($dyn_uni);
  80.449 ns (0 allocations: 0 bytes)

julia> @btime g($unitful);
  29.666 μs (42 allocations: 1.91 KiB)
```

Here, only the DynamicUnits `Quantity` results in a function that is type stable,
while the Unitful `Quantity` results in the compiler having to do type inference at runtime.



## Usage

You can create a `Quantity` object with a value and keyword arguments for the powers of the physical dimensions
(`mass`, `length`, `time`, `current`, `temperature`, `luminosity`, `amount`):

```julia
julia> x = Quantity(0.2, mass=1, length=0.5)
0.2 𝐋^(1//2) 𝐌^1

julia> y = Quantity(10.2, mass=2, time=-2)
10.2 𝐌^2 𝐓^(-2)
```

Elementary calculations with `+, -, *, /, ^, sqrt, cbrt` are supported:

```julia
julia> x * y
2.04 𝐋^(1//2) 𝐌^3 𝐓^(-2)

julia> x / y
0.019607843137254905 𝐋^(1//2) 𝐌^(-1) 𝐓^2

julia> x ^ 3
0.008000000000000002 𝐋^(3//2) 𝐌^3

julia> x ^ -1
5.0 𝐋^(-1//2) 𝐌^(-1)

julia> sqrt(x)
0.4472135954999579 𝐋^(1//4) 𝐌^(1//2)

julia> x ^ 1.5
0.0894427190999916 𝐋^(3//4) 𝐌^(3//2)
```

Each of these values has the same type, thus obviating the need for type inference at runtime.

Furthermore, we can do dimensional analysis automatically:

```julia
julia> x + 3 * x
0.8 𝐋^(1//2) 𝐌^1

julia> x + y
INVALID
```

We can see the second one has `valid(quantity) == false`. This doesn't throw an error by default, as it allows for stable return values.

The dimensions of a `Quantity` can be accessed either with `dimension(quantity)` for the entire `Dimensions` object:

```julia
julia> dimension(x)
𝐋^(1//2) 𝐌^1
```

or with `umass`, `ulength`, etc., for the various dimensions:

```julia
julia> umass(x)
1//1

julia> ulength(x)
1//2
```

Finally, you can strip units with `ustrip`:
    
```julia
julia> ustrip(x)
0.2
```

## Units

Despite the name, DynamicUnits does not actually work with units. Instead, it works with *dimensions*.
You can use Unitful to parse units, and use the DynamicUnits->Unitful extension for conversion:

```julia
julia> using Unitful: Unitful, @u_str

julia> x = 0.5u"km/s"
0.5 km s⁻¹

julia> y = convert(DynamicUnits.Quantity, x)
500.0 𝐋^1 𝐓^(-1)

julia> y2 = y^2 * 0.3
75000.0 𝐋^2 𝐓^(-2)

julia> x2 = convert(Unitful.Quantity, y2)
75000.0 m² s⁻²

julia> x^2*0.3 == x2
true
```

## Vectors

There is not a separate class for vectors, but you can create units
like so:

```julia
julia> randn(5) .* Dimensions(mass=2/5, length=2)
5-element Vector{Quantity{Float64}}:
 -0.72119725412798 𝐋^2 𝐌^(2//5)
 0.6443068291470538 𝐋^2 𝐌^(2//5)
 1.2137320667123697 𝐋^2 𝐌^(2//5)
 0.5125746727860678 𝐋^2 𝐌^(2//5)
 -0.6511788444561991 𝐋^2 𝐌^(2//5)
```

Because it is type stable, you can have mixed units in a vector too:

```julia
julia> v = [Quantity(randn(), mass=rand(0:5), length=rand(0:5)) for _=1:5]
5-element Vector{Quantity{Float64}}:
 0.6531745868307951 
 0.5260730397041357 𝐋^2 𝐌^5
 1.0827471975303913 𝐌^1
 1.5524518860763528 𝐌^1
 0.5376635007504901 𝐋^3 𝐌^1
```
