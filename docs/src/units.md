# Units

The two main functions for working with units are `uparse` and `u_str`:

```@docs
@u_str
uparse
```

## Available units

The base SI units are as follows.
You can either use the `@u_str` macro like `1.5u"m"`,
or simply import these explicitly from the package
with `using DynamicQuantities: m`.

```@docs
Units.m
Units.kg
Units.s
Units.A
Units.K
Units.cd
Units.mol
```

### Derived units

Several derived SI units are available as well:

```@autodocs
Modules = [Units]
Order = [:constant]
Filter = t -> t ∉ (Units.m, Units.kg, Units.s, Units.A, Units.K, Units.cd, Units.mol)
```

## Custom Units

You can define custom units with the `@register_unit` macro:

```@docs
@register_unit
```

## Affine Units

DynamicQuantities also supports affine units like Celsius and Fahrenheit through the `AffineUnit{R}` type and the `ua` string macro.
For example,

```julia
# Define temperature in Celsius
room_temp = 22ua"degC"    # 295.15 K

# Define temperature in Fahrenheit
freezing = 32ua"degF"     # 273.15 K

# Can take differences normally, as these are now regular Quantities:
room_temp - freezing
# 22 K
```

Note there are some subtleties about working with these:

```@docs
@ua_str
aff_uparse
DynamicQuantities.AffineUnit
```

Currently, the only supported affine units are:

- `°C` or `degC` - Degrees Celsius
- `°F` or `degF` - Degrees Fahrenheit
