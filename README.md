<div align="center">

![logo](https://github.com/SymbolicML/DynamicQuantities.jl/assets/7593028/a278d0c1-2f95-416b-ba04-82750074146b)

[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicQuantities.jl/dev/)
[![Build Status](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://coveralls.io/repos/github/SymbolicML/DynamicQuantities.jl/badge.svg?branch=main)](https://coveralls.io/github/SymbolicML/DynamicQuantities.jl?branch=main)

</div>
  
DynamicQuantities defines a simple statically-typed `Quantity` type for Julia.
Physical dimensions are stored as a *value*, as opposed to a parametric type, as in [Unitful.jl](https://github.com/PainterQubits/Unitful.jl).
This is done to allow for calculations where physical dimensions are not known at compile time.

- [Performance](#performance)
- [Usage](#usage)
- [Units](#units)
- [Types](#types)
- [Vectors](#vectors)

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

julia> @btime f($dyn_uni, 1);
  8.759 ns (0 allocations: 0 bytes)

julia> @btime f($unitful, 1);
  30.083 μs (42 allocations: 1.91 KiB)
```

**(Note the μ and n.)**
Here, the DynamicQuantities quantity object allows the compiler to build a function that is type stable,
while the Unitful quantity object, which stores its dimensions in the type, requires type inference at runtime.

However, if the dimensions in your function *can* be inferred by the compiler,
then you can get better speeds with Unitful:

```julia
julia> g(x) = x ^ 2 * 0.3;

julia> @btime g($dyn_uni);
  10.051 ns (0 allocations: 0 bytes)

julia> @btime g($unitful);
  2.000 ns (0 allocations: 0 bytes)
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

Furthermore, we can do dimensional analysis by detecting `DimensionError`:

```julia
julia> x + 3 * x
1.2 𝐋 ¹ᐟ² 𝐌 ¹

julia> x + y
ERROR: DimensionError: 0.3 𝐋 ¹ᐟ² 𝐌 ¹ and 10.2 𝐌 ² 𝐓 ⁻² have different dimensions
```

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

## Types

Both the `Quantity`'s values and dimensions are of arbitrary type.
By default, dimensions are stored as a `DynamicQuantities.FixedRational{Int32,C}`
object, which represents a rational number
with a fixed denominator `C`. This is much faster than `Rational`.

```julia
julia> typeof(Quantity(0.5, mass=1))
Quantity{Float64, FixedRational{Int32, 25200}
```

You can change the type of the value field by initializing with a value
of the desired type.

```julia
julia> typeof(Quantity(Float16(0.5), mass=1, length=1))
Quantity{Float16, FixedRational{Int32, 25200}}
```

For many applications, `FixedRational{Int8,6}` will suffice,
and can be faster as it means the entire `Dimensions`
struct will fit into 64 bits.
You can change the type of the dimensions field by passing
the type you wish to use as the second argument to `Quantity`:

```julia
julia> using DynamicQuantities

julia> R8 = DynamicQuantities.FixedRational{Int8,6};

julia> R32 = DynamicQuantities.FixedRational{Int32,2^4 * 3^2 * 5^2 * 7};  # Default

julia> q8 = [Quantity(randn(), R8, length=rand(-2:2)) for i in 1:1000];

julia> q32 = [Quantity(randn(), R32, length=rand(-2:2)) for i in 1:1000];

julia> f(x) = @. x ^ 2 * 0.5;

julia> @btime f($q8);
  7.750 μs (1 allocation: 15.75 KiB)

julia> @btime f($q32);
  8.417 μs (2 allocations: 39.11 KiB)
```

## Vectors

There is not a separate class for vectors, but you can create units
like so:

```julia
julia> randn(5) .* Dimensions(mass=2/5, length=2)
5-element Vector{Quantity{Float64, FixedRational{Int32, 25200}}}:
 -0.6450221578668845 𝐋 ² 𝐌 ²ᐟ⁵
 0.4024829670050946 𝐋 ² 𝐌 ²ᐟ⁵
 0.21478863605789672 𝐋 ² 𝐌 ²ᐟ⁵
 0.0719774550969669 𝐋 ² 𝐌 ²ᐟ⁵
 -1.4231241943420674 𝐋 ² 𝐌 ²ᐟ⁵
```

Because it is type stable, you can have mixed units in a vector too:

```julia
julia> v = [Quantity(randn(), mass=rand(0:5), length=rand(0:5)) for _=1:5]
5-element Vector{Quantity{Float64, FixedRational{Int32, 25200}}}:
 2.2054411324716865 𝐌 ³
 -0.01603602425887379 𝐋 ⁴ 𝐌 ³
 1.4388184352393647 
 2.382303019892503 𝐋 ² 𝐌 ¹
 0.6071392594021706 𝐋 ⁴ 𝐌 ⁴
```
