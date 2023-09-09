module DynamicQuantities

import TOML: parsefile

const PACKAGE_VERSION = try
    let project = parsefile(joinpath(pkgdir(@__MODULE__), "Project.toml"))
        VersionNumber(project["version"])
    end
catch
    VersionNumber(0, 0, 0)
end

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

import PackageExtensionCompat: @require_extensions
import .Units
import .Constants
import .UnitsParse: uparse, @u_str

function __init__()
    @require_extensions
end

end
