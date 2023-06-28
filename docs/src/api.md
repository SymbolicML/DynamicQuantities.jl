# Usage

## Types

```@docs
Quantity
Dimensions
```

There are also abstract types available. There are no required
functions to build an interface, most relevant functions are
defined on the abstract functions (including constructors).

```@docs
AbstractDimensions
AbstractQuantity
```

Note also that the `Quantity` object can take a custom `AbstractDimensions`
as input, so there is often no need to subtype `AbstractQuantity` separately.

## Utilities

The two main general utilities for working
with quantities are `ustrip` and `dimension`:

```@docs
ustrip
dimension
```

### Accessing dimensions

Utility functions to extract specific dimensions are as follows:

```@docs
ulength
umass
utime
ucurrent
utemperature
uluminosity
uamount
```

```@autodocs
Modules = [DynamicQuantities]
Pages   = ["utils.jl"]
Filter  = t -> !(t in [ustrip, dimension, ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount])
```

## Units

The two main functions for working with units are `uparse` and `u_str`:

```@docs
@u_str
uparse
```

### Available units

The base SI units are as follows.
Instead of calling directly, it is recommended to access them via
the `@u_str` macro, which evaluates the expression
in a namespace with all the units available.

```@docs
Units.m
Units.g
Units.s
Units.A
Units.K
Units.cd
Units.mol
```

Several derived SI units are available as well:

```@docs
Units.Hz
Units.N
Units.Pa
Units.J
Units.W
Units.C
Units.V
Units.F
Units.Ω
Units.T
Units.L
Units.bar
```

## Internals

### FixedRational

```@docs
DynamicQuantities.FixedRational
DynamicQuantities.denom
```

