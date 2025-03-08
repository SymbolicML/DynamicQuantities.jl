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

You can also access these from the exported modules
`SymbolicUnits` for the units and `SymbolicConstants`.
The same units and constants are available as for `u"..."`,
simply in the symbolic form.


To convert a quantity to its regular base SI units, use `uexpand`:

```@docs
uexpand
ustripexpand
```

To convert a quantity in regular base SI units to corresponding symbolic units, use `uconvert`:

```@docs
uconvert
```
