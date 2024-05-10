using TestItems: @testitem
using TestItemRunner
import Ratios: SimpleRatio

if !hasmethod(round, Tuple{Int, SimpleRatio{Int}})
    Base.round(::Type{T}, x::SimpleRatio) where {T} = round(T, x.num // x.den)
end

@testset "Test upreferred disallowed" tags=[:upreferred] begin
    include("test_ban_upreferred.jl")
end

@testitem "Unitful.jl integration tests" begin
    include("test_unitful.jl")
end
@testitem "ScientificTypes.jl integration tests" begin
    include("test_scitypes.jl")
end
@testitem "Measurements.jl integration tests" begin
    include("test_measurements.jl")
end
## Broken; see https://github.com/SymbolicML/DynamicQuantities.jl/issues/118
# @testitem "Meshes.jl integration tests" begin
#     include("test_meshes.jl")
# end
include("unittests.jl")

@testitem "LinearAlgebra.jl integration tests" begin
    include("test_linearalgebra.jl")
end
@testitem "Aqua tests" begin
    include("test_aqua.jl")
end

if parse(Bool, get(ENV, "DQ_TEST_UPREFERRED", "false"))
    @eval @run_package_tests filter=t -> :upreferred in t.tags
else
    @eval @run_package_tests filter=t -> !(:upreferred in t.tags)
end
