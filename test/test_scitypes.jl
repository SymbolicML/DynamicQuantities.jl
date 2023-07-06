using DynamicQuantities
using ScientificTypes

x = 1.0u"m/s"

@test scitype(x) <: Continuous
@test scitype([x]) <: AbstractVector{<:Continuous}
@test scitype(Quantity{Int}(x)) <: Count
@test scitype(randn(32) .* u"m/s") <: AbstractVector{<:Continuous}

X = (; x=randn(32) .* u"m/s")

@test scitype(X) <: Table{<:AbstractVector{<:Continuous}}

sch = schema(X)

@test first(sch.names) == :x
@test first(sch.scitypes) == Continuous
@test first(sch.types) <: Quantity{Float64}
