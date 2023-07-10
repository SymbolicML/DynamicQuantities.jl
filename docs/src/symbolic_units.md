# Symbolic Dimensions

Whereas `u"..."` will automatically convert all units to the same
base SI units, `us"..."` will not. This uses the `SymbolicDimensions`
type, which is a subtype of `AbstractDimensions` that stores the
dimensions symbolically. This is useful for keeping track of the
original units and constants in a user-entered expression.

The two main functions for working with symbolic
units are `sym_uparse` and `us_str`:

```@docs
@us_str
sym_uparse
```

To convert a quantity to its regular base SI units, use `expand_units`:

```@docs
expand_units
```
