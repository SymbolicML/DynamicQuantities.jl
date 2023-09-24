<div align="center">

![logo](https://github.com/SymbolicML/DynamicQuantities.jl/assets/7593028/a278d0c1-2f95-416b-ba04-82750074146b)

[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicQuantities.jl/dev/)
[![Build Status](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/SymbolicML/DynamicQuantities.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://coveralls.io/repos/github/SymbolicML/DynamicQuantities.jl/badge.svg?branch=main)](https://coveralls.io/github/SymbolicML/DynamicQuantities.jl?branch=main)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

</div>
  
DynamicQuantities defines a simple statically-typed `Quantity` type for Julia.
Physical dimensions are stored as a *value*, as opposed to a parametric type, as in [Unitful.jl](https://github.com/PainterQubits/Unitful.jl).
This can greatly improve both runtime performance, by avoiding type instabilities, and startup time, as it avoids overspecializing methods.

- [Performance](#performance)
- [Usage](#usage)
  - [Constants](#constants)
  - [Symbolic Units](#symbolic-units)
  - [Arrays](#arrays)
  - [Unitful](#unitful)
- [Types](#types)
- [Vectors](#vectors)

## Performance

DynamicQuantities can greatly outperform Unitful
when the compiler cannot infer dimensions in a function:

```julia
julia> using BenchmarkTools, DynamicQuantities; import Unitful

julia> dyn_uni = 0.2u"m/s"
0.2 m s⁻¹

julia> unitful = convert(Unitful.Quantity, dyn_uni)
0.2 m s⁻¹

julia> f(x, i) = x ^ i * 0.3;

julia> @btime f($dyn_uni, 1);
  2.708 ns (0 allocations: 0 bytes)

julia> @btime f($unitful, 1);
  2.597 μs (30 allocations: 1.33 KiB)
```

**Note the μ and n: this is a 1000x speedup!**
Here, the DynamicQuantities quantity object allows the compiler to build a function that is type stable,
while the Unitful quantity object, which stores its dimensions in the type, requires type inference at runtime.

However, if the dimensions in your function *can* be inferred by the compiler,
then you can get better speeds with Unitful:

```julia
julia> g(x) = x ^ 2 * 0.3;

julia> @btime g($dyn_uni);
  1.791 ns (0 allocations: 0 bytes)

julia> @btime g($unitful);
  1.500 ns (0 allocations: 0 bytes)
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

### Constants

There are a variety of physical constants accessible
via the `Constants` submodule:

```julia
julia> Constants.c
2.99792458e8 m s⁻¹
```

These can also be used inside the `u"..."` macro:

```julia
julia> u"Constants.c * Hz"
2.99792458e8 m s⁻²
```

For the full list, see the [docs](https://symbolicml.org/DynamicQuantities.jl/dev/constants/).


### Symbolic Units

You can also choose to not eagerly convert to SI base units,
instead leaving the units as the user had written them.
For example:

```julia
julia> q = 100us"cm * kPa"
100.0 cm kPa

julia> q^2
10000.0 cm² kPa²
```

You can convert to regular SI base units with
`expand_units`:

```julia
julia> expand_units(q^2)
1.0e6 kg² s⁻⁴
```

This also works with constants:

```julia
julia> x = us"Constants.c * Hz"
1.0 Hz c

julia> x^2
1.0 Hz² c²

julia> expand_units(x^2)
8.987551787368176e16 m² s⁻⁴
```

You can also convert a quantity in regular base SI units to symbolic units with `as_units`:
```julia
julia> as_units(5e-9u"m", us"nm") # can also write 5e-9u"m" |> as_units(us"nm")
5.0 nm
```

### Arrays

For working with an array of quantities that have the same dimensions,
you can use a `QuantityArray`:

```julia
julia> ar = QuantityArray(rand(3), u"m/s")
3-element QuantityArray(::Vector{Float64}, ::Quantity{Float64, Dimensions{DynamicQuantities.FixedRational{Int32, 25200}}}):
 0.2729202669351497 m s⁻¹
 0.992546340360901 m s⁻¹
 0.16863543422972482 m s⁻¹
```

This `QuantityArray` is a subtype `<:AbstractArray{Quantity{Float64,Dimensions{...}},1}`,
meaning that indexing a specific element will return a `Quantity`:

```julia
julia> ar[2]
0.992546340360901 m s⁻¹

julia> ar[2] *= 2
1.985092680721802 m s⁻¹

julia> ar[2] += 0.5u"m/s"
2.485092680721802 m s⁻¹
```

This also has a custom broadcasting interface which
allows the compiler to avoid redundant dimension calculations,
relative to if you had simply used an array of quantities:

```julia
julia> f(v) = v^2 * 1.5;

julia> @btime $f.(xa) setup=(xa = randn(100000) .* u"km/s");
  109.500 μs (2 allocations: 3.81 MiB)

julia> @btime $f.(qa) setup=(xa = randn(100000) .* u"km/s"; qa = QuantityArray(xa));
  50.917 μs (3 allocations: 781.34 KiB)
```

So we can see the `QuantityArray` version saves on both time and memory.

### Unitful

DynamicQuantities allows you to convert back and forth from Unitful.jl:

```julia
julia> using Unitful: Unitful, @u_str; import DynamicQuantities

julia> x = 0.5u"km/s"
0.5 km s⁻¹

julia> y = convert(DynamicQuantities.Quantity, x)
500.0 m s⁻¹

julia> y2 = y^2 * 0.3
75000.0 m² s⁻²

julia> x2 = convert(Unitful.Quantity, y2)
75000.0 m² s⁻²

julia> x^2*0.3 == x2
true
```

## Types

Both a `Quantity`'s values and dimensions are of arbitrary type.
By default, dimensions are stored as a `Dimensions{FixedRational{Int32,C}}`
object, whose exponents are stored as rational numbers
with a fixed denominator `C`. This is much faster than `Rational`.

```julia
julia> typeof(0.5u"kg")
Quantity{Float64, Dimensions{FixedRational{Int32, 25200}}}
```

You can change the type of the value field by initializing with a value
explicitly of the desired type.

```julia
julia> typeof(Quantity(Float16(0.5), mass=1, length=1))
Quantity{Float16, Dimensions{FixedRational{Int32, 25200}}}
```

or by conversion:

```julia
julia> typeof(convert(Quantity{Float16}, 0.5u"m/s"))
Quantity{Float16, Dimensions{FixedRational{Int32, 25200}}}
```

For many applications, `FixedRational{Int8,6}` will suffice,
and can be faster as it means the entire `Dimensions`
struct will fit into 64 bits.
You can change the type of the dimensions field by passing
the type you wish to use as the second argument to `Quantity`:

```julia
julia> using DynamicQuantities

julia> R8 = Dimensions{DynamicQuantities.FixedRational{Int8,6}};

julia> R32 = Dimensions{DynamicQuantities.FixedRational{Int32,2^4 * 3^2 * 5^2 * 7}};  # Default

julia> q8 = [Quantity(randn(), R8, length=rand(-2:2)) for i in 1:1000];

julia> q32 = [Quantity(randn(), R32, length=rand(-2:2)) for i in 1:1000];

julia> f(x) = @. x ^ 2 * 0.5;

julia> @btime f($q8);
  7.750 μs (1 allocation: 15.75 KiB)

julia> @btime f($q32);
  8.417 μs (2 allocations: 39.11 KiB)
```
