# Units

The two main functions for working with units are `uparse` and `u_str`:

```@docs
@u_str
uparse
```

## Available units

The base SI units are as follows.
Instead of calling directly, it is recommended to access them via
the `@u_str` macro, which evaluates the expression
in a namespace with all the units available.

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

## Custom Units

You can define custom units with the `@register_unit` macro:

```@docs
@register_unit
```
