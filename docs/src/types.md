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

Whereas `Quantity` is subtyped to `Number`,
a more general type of quantity is `GenericQuantity`,
which is subtyped to `Any`.

```@docs
GenericQuantity
AbstractGenericQuantity
```

In the other direction, there is also `RealQuantity`,
which is subtyped to `Real`.

```@docs
RealQuantity
AbstractRealQuantity
```

More general, these are each contained in the following:

```@docs
UnionAbstractQuantity
DynamicQuantities.ABSTRACT_QUANTITY_TYPES
```

## Custom behavior in abstract quantities

There are a few functions you may need to overload
when subtyping `AbstractDimensions`, `AbstractQuantity`,
or `AbstractGenericQuantity`.

```@docs
constructorof
with_type_parameters
dimension_names
```
