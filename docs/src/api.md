# Utilities

The two main general utilities for working
with quantities are `ustrip` and `dimension`:

```@docs
ustrip
dimension
```

## Accessing dimensions

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

## Internals

### FixedRational

```@docs
DynamicQuantities.FixedRational
DynamicQuantities.denom
```
