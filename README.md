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

julia> dyn_uni = 0.2u"m^0.5 * kg * mol^3"
0.2 m¹ᐟ² kg mol³

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

You can create a `Quantity` object 
by using the convenience macro `u"..."`:

```julia
julia> x = 0.3u"km/s"
300.0 m s⁻¹

julia> y = 42 * u"kg"
42.0 kg

julia> room_temp = 100u"kPa"
100000.0 m⁻¹ kg s⁻²
```

This supports a wide range of SI base and derived units, with common
prefixes.

You can also construct values explicitly with the `Quantity` type,
with a value and keyword arguments for the powers of the physical dimensions
(`mass`, `length`, `time`, `current`, `temperature`, `luminosity`, `amount`):

```julia
julia> x = Quantity(300.0, length=1, time=-1)
300.0 m s⁻¹
```

Elementary calculations with `+, -, *, /, ^, sqrt, cbrt, abs` are supported:

```julia
julia> x * y
12600.0 m kg s⁻¹

julia> x / y
7.142857142857143 m kg⁻¹ s⁻¹

julia> x ^ 3
2.7e7 m³ s⁻³

julia> x ^ -1
0.0033333333333333335 m⁻¹ s

julia> sqrt(x)
17.320508075688775 m¹ᐟ² s⁻¹ᐟ²

julia> x ^ 1.5
5196.152422706632 m³ᐟ² s⁻³ᐟ²
```

Each of these values has the same type, which means we don't need to perform type inference at runtime.

Furthermore, we can do dimensional analysis by detecting `DimensionError`:

```julia
julia> x + 3 * x
1.2 m¹ᐟ² kg

julia> x + y
ERROR: DimensionError: 0.3 m¹ᐟ² kg and 10.2 kg² s⁻² have incompatible dimensions
```

The dimensions of a `Quantity` can be accessed either with `dimension(quantity)` for the entire `Dimensions` object:

```julia
julia> dimension(x)
m¹ᐟ² kg
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

## Unitful

DynamicQuantities works with quantities that are exclusively
represented by their SI base units. This gives us type stability
and greatly improves performance.

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
julia> typeof(0.5u"kg")
Quantity{Float64, FixedRational{Int32, 25200}
```

You can change the type of the value field by initializing with a value
explicitly of the desired type.

```julia
julia> typeof(Quantity(Float16(0.5), mass=1, length=1))
Quantity{Float16, FixedRational{Int32, 25200}}
```

or by conversion:

```julia
julia> typeof(convert(Quantity{Float16}, 0.5u"m/s"))
Quantity{Float16, DynamicQuantities.FixedRational{Int32, 25200}}
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
julia> randn(5) .* u"m/s"
5-element Vector{Quantity{Float64, DynamicQuantities.FixedRational{Int32, 25200}}}:
 1.1762086954956399 m s⁻¹
 1.320811324040591 m s⁻¹
 0.6519033652437799 m s⁻¹
 0.7424822374423569 m s⁻¹
 0.33536928068133726 m s⁻¹
```

Because it is type stable, you can have mixed units in a vector too:

```julia
julia> v = [Quantity(randn(), mass=rand(0:5), length=rand(0:5)) for _=1:5]
5-element Vector{Quantity{Float64, DynamicQuantities.FixedRational{Int32, 25200}}}:
 0.4309293892461158 kg⁵
 1.415520139801276
 1.2179414706524276 m³ kg⁴
 -0.18804207255117408 m³ kg⁵
 0.52123911329638 m³ kg²
```
