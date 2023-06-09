using SafeTestsets, DynamicQuantities
import Ratios: SimpleRatio

@static if !hasmethod(round, Tuple{Int, SimpleRatio{Int}})
    @eval Base.round(T, x::SimpleRatio) = round(T, x.num // x.den)
end


@safetestset "Unit tests" begin
    include("unittests.jl")
end

@safetestset "Unitful.jl integration tests" begin
    include("test_unitful.jl")
end

@safetestset "doctests" begin
    using Documenter, DynamicQuantities
    DocMeta.setdocmeta!(DynamicQuantities, :DocTestSetup, :(using Unitful, DynamicQuantities); recursive=true)
    Documenter.doctest(DynamicQuantities)
end


