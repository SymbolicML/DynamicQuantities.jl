using TestItems: @testitem
using TestItemRunner
import Ratios: SimpleRatio

#=
Run using:
julia --startup-file=no --depwarn=yes --threads=auto -e 'using Coverage; clean_folder(\"src\"); clean_folder(\"test\")'
julia --startup-file=no --depwarn=yes --threads=auto --code-coverage=user --project=. -e 'using Pkg; Pkg.test(coverage=true)'
julia --startup-file=no --depwarn=yes --threads=auto coverage.jl
=#

Base.round(::Type{T}, x::SimpleRatio) where {T} = round(T, x.num // x.den)

@eval @testitem "Test initial imports" begin
    include("test_initial_imports.jl")
end

@eval @testitem "Test upreferred disallowed" tags=[:upreferred] begin
    include("test_ban_upreferred.jl")
end

@testitem "Unitful.jl integration tests" begin
    using DispatchDoctor
    allow_unstable() do
        include("test_unitful.jl")
    end
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
@testitem "Assorted unittests" begin
    include("unittests.jl")
end

@testitem "Aqua tests" begin
    include("test_aqua.jl")
end

if parse(Bool, get(ENV, "DQ_TEST_UPREFERRED", "false"))
    @eval @run_package_tests filter=t -> :upreferred in t.tags
else
    @eval @run_package_tests filter=t -> !(:upreferred in t.tags)
end
