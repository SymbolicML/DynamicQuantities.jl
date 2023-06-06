using SafeTestsets

@safetestset "Unit tests" begin
    include("unittests.jl")
end

@safetestset "Unitful.jl integration tests" begin
    include("unitful.jl")
end
