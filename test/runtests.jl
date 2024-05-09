using SafeTestsets
import Ratios: SimpleRatio

if !hasmethod(round, Tuple{Int, SimpleRatio{Int}})
    Base.round(::Type{T}, x::SimpleRatio) where {T} = round(T, x.num // x.den)
end

if parse(Bool, get(ENV, "DQ_TEST_UPREFERRED", "false"))
    @safetestset "Test upreferred disallowed" begin
        include("test_ban_upreferred.jl")
    end
else
    @safetestset "Unitful.jl integration tests" begin
        include("test_unitful.jl")
    end
    @safetestset "ScientificTypes.jl integration tests" begin
        include("test_scitypes.jl")
    end
    @safetestset "Measurements.jl integration tests" begin
        include("test_measurements.jl")
    end
    ## Broken; see https://github.com/SymbolicML/DynamicQuantities.jl/issues/118
    # @safetestset "Meshes.jl integration tests" begin
    #     include("test_meshes.jl")
    # end
    @safetestset "Unit tests" begin
        include("unittests.jl")
    end
    @safetestset "LinearAlgebra.jl integration tests" begin
        include("test_linearalgebra.jl")
    end
    @safetestset "Aqua tests" begin
        include("test_aqua.jl")
    end
end
