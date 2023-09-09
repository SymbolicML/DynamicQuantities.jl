using DynamicQuantities
using ScientificTypes
import ScientificTypes as ST
import ScientificTypesBase as STB

x = 1.0u"m/s"

@test scitype(x) <: Continuous
@test scitype([x]) <: AbstractVector{<:Continuous}
@test scitype(Quantity{Int}(x)) <: Count
@test scitype(randn(32) .* u"m/s") <: AbstractVector{<:Continuous}
@test STB.Scitype(typeof(x), ST.DefaultConvention()) <: Continuous

X = (; x=randn(32) .* u"m/s")

@test scitype(X) <: Table{<:AbstractVector{<:Continuous}}

sch = schema(X)

@test first(sch.names) == :x
@test first(sch.scitypes) == Continuous
@test first(sch.types) <: Quantity{Float64}

@test first(schema((; x=rand(1:10, 5) .* Quantity{Int}(u"m/s"))).scitypes) == Count
