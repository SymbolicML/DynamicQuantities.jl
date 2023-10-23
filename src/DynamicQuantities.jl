module DynamicQuantities

export Units, Constants
export AbstractDimensions, AbstractQuantity, AbstractGenericQuantity, AbstractUnionQuantity
export Quantity, GenericQuantity, Dimensions, SymbolicDimensions, QuantityArray, DimensionError
export ustrip, dimension
export ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount
export uparse, @u_str, sym_uparse, @us_str, uexpand, uconvert

include("fixed_rational.jl")
include("types.jl")
include("utils.jl")
include("math.jl")
include("arrays.jl")
include("units.jl")
include("constants.jl")
include("uparse.jl")
include("symbolic_dimensions.jl")
include("disambiguities.jl")

include("deprecated.jl")
export expand_units

import PackageExtensionCompat: @require_extensions
import .Units
import .Constants
import .UnitsParse: uparse, @u_str

function __init__()
    @require_extensions
end

end
