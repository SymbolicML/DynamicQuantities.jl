module DynamicQuantities

export Quantity, Dimensions, ustrip, dimension, valid
export ulength, umass, utime, ucurrent, utemperature, uluminosity, uamount

import Ratios: SimpleRatio

include("types.jl")
include("utils.jl")
include("math.jl")

import Requires: @init, @require

if !isdefined(Base, :get_extension)
    @init @require Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d" include("../ext/DynamicQuantitiesUnitfulExt.jl")
end

end
