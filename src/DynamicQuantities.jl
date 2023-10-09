module DynamicQuantities

export Units, Constants
export AbstractQuantity, AbstractDimensions
export Quantity, Dimensions, SymbolicDimensions, QuantityArray, DimensionError
export ustrip, dimension
export ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount
export uparse, @u_str, sym_uparse, @us_str, expand_units

include("fixed_rational.jl")
include("types.jl")
include("utils.jl")
include("math.jl")
include("arrays.jl")
include("units.jl")
include("constants.jl")
include("uparse.jl")
include("symbolic_dimensions.jl")

import .Units
import .Constants
import .UnitsParse: uparse, @u_str

if !isdefined(Base, :get_extension)
using Requires
end
@static if !isdefined(Base, :get_extension)
function __init__()
    @require LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e" include("../ext/DynamicQuantitiesLinearAlgebraExt.jl")
    @require Measurements = "eff96d63-e80a-5855-80a2-b1b0885c5ab7" include("../ext/DynamicQuantitiesMeasurementsExt.jl")
    @require ScientificTypes = "321657f4-b219-11e9-178b-2701a2544e81" include("../ext/DynamicQuantitiesScientificTypesExt.jl")
    @require Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d" include("../ext/DynamicQuantitiesUnitfulExt.jl")
end
end

end
