using SafeTestsets
import Ratios: SimpleRatio

@static if !hasmethod(round, Tuple{Int, SimpleRatio{Int}})
    @eval Base.round(T, x::SimpleRatio) = round(T, x.num // x.den)
end

if parse(Bool, get(ENV, "DQ_TEST_UPREFERRED", "false"))
    @safetestset "Test upreferred disallowed" begin
        include("test_ban_upreferred.jl")
    end
else
    @safetestset "Unitful.jl integration tests" begin
        include("test_unitful.jl")
    end
    @safetestset "Unit tests" begin
        include("unittests.jl")
    end
end