# Types

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

## Symbolic dimensions

Another type which subtypes `AbstractDimensions` is `SymbolicDimensions`:

```@docs
SymbolicDimensions
```

## Arrays

```@docs
QuantityArray
```

## Generic quantities

```@docs
GenericQuantity
AbstractGenericQuantity
AbstractUnionQuantity
```
