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
