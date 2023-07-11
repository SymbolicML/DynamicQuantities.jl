module DynamicQuantities

export Units, Constants
export AbstractQuantity, AbstractDimensions
export Quantity, Dimensions, SymbolicDimensions, DimensionError
export ustrip, dimension, valid
export ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount
export uparse, @u_str, sym_uparse, @us_str, expand_units

include("fixed_rational.jl")
include("types.jl")
include("utils.jl")
include("math.jl")
include("units.jl")
include("constants.jl")
include("uparse.jl")
include("symbolic_dimensions.jl")

import Requires: @init, @require
import .Units
import .Constants
import .UnitsParse: uparse, @u_str

if !isdefined(Base, :get_extension)
    @init @require Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d" include("../ext/DynamicQuantitiesUnitfulExt.jl")
    @init @require ScientificTypes = "321657f4-b219-11e9-178b-2701a2544e81" begin
        @require ScientificTypesBase = "30f210dd-8aff-4c5f-94ba-8e64358c1161" include("../ext/DynamicQuantitiesScientificTypesExt.jl")
    end
end

end
