using SafeTestsets
import Ratios: SimpleRatio

@safetestset "QuadGK integration tests" begin
    include("test_quadgk.jl")
end
