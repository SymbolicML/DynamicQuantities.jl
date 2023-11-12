module DynamicQuantitiesDiffEqBaseExt

using DynamicQuantities: UnionAbstractQuantity, ustrip

import DiffEqBase

DiffEqBase.value(x::UnionAbstractQuantity) = ustrip(x)
DiffEqBase.recursive_length(u::UnionAbstractQuantity) = length(u)
DiffEqBase.recursive_length(u::QuantityArray) = length(u)

# @inline function DiffEqBase.UNITLESS_ABS2(x::UnionAbstractQuantity)
#     abs(DiffEqBase.value(x))
# end
# function DiffEqBase.abs2_and_sum(x::DynamicQuantities.Quantity, y::Float64)
#     reduce(Base.add_sum, DiffEqBase.value(x), init = zero(real(DiffEqBase.value(x)))) +
#     reduce(Base.add_sum, y, init = zero(real(DiffEqBase.value(eltype(y)))))
# end

# Base.sign(x::DynamicQuantities.Quantity) = Base.sign(DiffEqBase.value(x))

# function DiffEqBase.prob2dtmin(prob; use_end_time = true)
#     DiffEqBase.prob2dtmin(prob.tspan, oneunit(first(prob.tspan)), use_end_time)
# end

# DiffEqBase.NAN_CHECK(x::DynamicQuantities.Quantity) = isnan(x)
# Base.zero(x::Array{T}) where {T<:DynamicQuantities.Quantity} = zero.(x)

# @inline function DiffEqBase.calculate_residuals(ũ, u₀, u₁, α, ρ, internalnorm, t)
#     @. DiffEqBase.calculate_residuals(ũ, u₀, u₁, α, ρ, internalnorm, t)
# end

# f(u, p, t) = u / t;
# problem = ODEProblem(f, [1.0u"km/s"], (0.0u"s", 1.0u"s"));
# sol = solve(problem, Tsit5(), dt = 0.1u"s")

end