# DynamicQuantities

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://symbolicml.org/DynamicQuantities.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicQuantities.jl/dev/)
[![Build Status](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://coveralls.io/repos/github/SymbolicML/DynamicQuantities.jl/badge.svg?branch=main)](https://coveralls.io/github/SymbolicML/DynamicQuantities.jl?branch=main)

This defines a simple statically-typed `Quantity` type for Julia.
Physical dimensions are stored as a *value*, as opposed to a parametric type, as in [Unitful.jl](https://github.com/PainterQubits/Unitful.jl).
This is done to allow for calculations where physical dimensions are not known at compile time.

## Performance

DynamicQuantities can greatly outperform Unitful
when the compiler cannot infer dimensions in a function:

```julia
julia> using BenchmarkTools, DynamicQuantities; import Unitful

julia> dyn_uni = Quantity(0.2, mass=1, length=0.5, amount=3)
0.2 𝐋 ¹ᐟ² 𝐌 ¹ 𝐍 ³

julia> unitful = convert(Unitful.Quantity, dyn_uni)
0.2 kg m¹ᐟ² mol³

julia> f(x, i) = x ^ i * 0.3;

julia> @btime f($dyn_uni, i) setup=(i=rand(1:10));
  9.384 ns (0 allocations: 0 bytes)

julia> @btime f($unitful, i) setup=(i=rand(1:10));
  29.667 μs (42 allocations: 1.91 KiB)
```

**(Note the μ and n.)**
Here, the DynamicQuantities quantity object allows the compiler to build a function that is type stable,
while the Unitful quantity object, which stores its dimensions in the type, requires type inference at runtime.

However, if the dimensions in your function *can* be inferred by the compiler,
then you can get better speeds with Unitful:

```julia
julia> g(x) = x ^ 2 * 0.3;

julia> @btime g($dyn_uni);
  6.083 ns (0 allocations: 0 bytes)

julia> @btime g($unitful);
  1.958 ns (0 allocations: 0 bytes)
```

While both of these are type stable,
because Unitful parametrizes the type on the dimensions, functions can specialize
to units and the compiler can optimize away units from the code.

## Usage

You can create a `Quantity` object with a value and keyword arguments for the powers of the physical dimensions
(`mass`, `length`, `time`, `current`, `temperature`, `luminosity`, `amount`):

```julia
julia> x = Quantity(0.3, mass=1, length=0.5)
0.3 𝐋 ¹ᐟ² 𝐌 ¹

julia> y = Quantity(10.2, mass=2, time=-2)
10.2 𝐌 ² 𝐓 ⁻²
```

Elementary calculations with `+, -, *, /, ^, sqrt, cbrt` are supported:

```julia
julia> x * y
3.0599999999999996 𝐋 ¹ᐟ² 𝐌 ³ 𝐓 ⁻²

julia> x / y
0.029411764705882353 𝐋 ¹ᐟ² 𝐌 ⁻¹ 𝐓 ²

julia> x ^ 3
0.027 𝐋 ³ᐟ² 𝐌 ³

julia> x ^ -1
3.3333333333333335 𝐋 ⁻¹ᐟ² 𝐌 ⁻¹

julia> sqrt(x)
0.5477225575051661 𝐋 ¹ᐟ⁴ 𝐌 ¹ᐟ²

julia> x ^ 1.5
0.1643167672515498 𝐋 ³ᐟ⁴ 𝐌 ³ᐟ²
```

Each of these values has the same type, thus obviating the need for type inference at runtime.

Furthermore, we can do dimensional analysis automatically:

```julia
julia> x + 3 * x
1.2 𝐋 ¹ᐟ² 𝐌 ¹

julia> x + y
INVALID
```

We can see the second one has `valid(quantity) == false`. This doesn't throw an error by default, as it allows for stable return values.

The dimensions of a `Quantity` can be accessed either with `dimension(quantity)` for the entire `Dimensions` object:

```julia
julia> dimension(x)
𝐋 ¹ᐟ² 𝐌 ¹
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

DynamicQuantities works with quantities which store physical dimensions and a value,
and does not directly provide a unit system.
However, performing calculations with physical dimensions
is actually equivalent to working with a standardized unit system.
Thus, you can use Unitful to parse units,
and then use the DynamicQuantities->Unitful extension for conversion:

```julia
julia> using Unitful: Unitful, @u_str

julia> x = 0.5u"km/s"
0.5 km s⁻¹

julia> y = convert(DynamicQuantities.Quantity, x)
500.0 𝐋 ¹ 𝐓 ⁻¹

julia> y2 = y^2 * 0.3
75000.0 𝐋 ² 𝐓 ⁻²

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
 -0.6450221578668845 𝐋 ² 𝐌 ²ᐟ⁵
 0.4024829670050946 𝐋 ² 𝐌 ²ᐟ⁵
 0.21478863605789672 𝐋 ² 𝐌 ²ᐟ⁵
 0.0719774550969669 𝐋 ² 𝐌 ²ᐟ⁵
 -1.4231241943420674 𝐋 ² 𝐌 ²ᐟ⁵
```

Because it is type stable, you can have mixed units in a vector too:

```julia
julia> v = [Quantity(randn(), mass=rand(0:5), length=rand(0:5)) for _=1:5]
5-element Vector{Quantity{Float64}}:
 2.2054411324716865 𝐌 ³
 -0.01603602425887379 𝐋 ⁴ 𝐌 ³
 1.4388184352393647 
 2.382303019892503 𝐋 ² 𝐌 ¹
 0.6071392594021706 𝐋 ⁴ 𝐌 ⁴
```
