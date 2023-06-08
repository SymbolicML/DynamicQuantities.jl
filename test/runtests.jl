using SafeTestsets, DynamicQuantities

@safetestset "Unit tests" begin
    include("unittests.jl")
end

@safetestset "Unitful.jl integration tests" begin
    include("unitful.jl")
end

@safetestset "doctests" begin
    using Documenter, DynamicQuantities
    DocMeta.setdocmeta!(DynamicQuantities, :DocTestSetup, :(using Unitful, DynamicQuantities); recursive=true)
    Documenter.doctest(DynamicQuantities)
end
