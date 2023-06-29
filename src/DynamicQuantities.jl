module DynamicQuantities

export AbstractQuantity, AbstractDimensions
export Quantity, Dimensions, DimensionError, ustrip, dimension, valid
export ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount
export uparse, @u_str

include("fixed_rational.jl")
include("lazy_float.jl")

include("types.jl")
include("utils.jl")
include("math.jl")
include("units.jl")

import Requires: @init, @require
import .Units: uparse, @u_str, DEFAULT_UNIT_TYPE

if !isdefined(Base, :get_extension)
    @init @require Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d" include("../ext/DynamicQuantitiesUnitfulExt.jl")
end

end
