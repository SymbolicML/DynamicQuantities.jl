module DynamicQuantitiesMakieExt

using DynamicQuantities: UnionAbstractQuantity, ustrip, dimension
using TestItems: @testitem

import Makie as M

M.expand_dimensions(::M.PointBased, y::AbstractVector{<:UnionAbstractQuantity}) = (keys(y), y)
M.create_dim_conversion(::Type{<:UnionAbstractQuantity}) = DQConversion()
M.MakieCore.should_dim_convert(::Type{<:UnionAbstractQuantity}) = true

unit_string(quantity::UnionAbstractQuantity) = string(dimension(quantity))

function unit_convert(::M.Automatic, x)
    x
end

function unit_convert(quantity::UnionAbstractQuantity, x::AbstractArray)
    # Note: unit_convert.(Ref(quantity), x) currently causes broadcasting error for `QuantityArray`s
    map(Base.Fix1(unit_convert, quantity), x)
end

function unit_convert(quantity::UnionAbstractQuantity, value)
    conv = ustrip(quantity, value)
    return Float64(conv)
end

# TODO: Maybe only allow symbolic units to avoid bugs?
"""
    DQConversion(unit=automatic; units_in_label=false)
Allows to plot arrays of DynamicQuantity objects into an axis.
# Arguments
- `unit=automatic`: sets the unit as conversion target. If left at automatic, the best unit will be chosen for all plots + values plotted to the axis (e.g. years for long periods, or km for long distances, or nanoseconds for short times).
- `units_in_label=true`: controls, whether plots are shown in the label_prefix of the axis labels, or in the tick labels
# Examples
```julia
using DynamicQuantities, CairoMakie
# DQConversion will get chosen automatically:
scatter(1:4, [1u"ns", 2u"ns", 3u"ns", 4u"ns"])
```
Fix unit to always use Meter & display unit in the xlabel:
```julia
# Temporary until this is upstreamed to Makie.jl
const DQConversion = Base.get_extension(DynamicQuantities, :DynamicQuantitiesMakieExt).DQConversion
dqc = DQConversion(us"m"; units_in_label=false)
scatter(1:4, [0.01u"km", 0.02u"km", 0.03u"km", 0.04u"km"]; axis=(dim2_conversion=dqc, xlabel="x (km)"))
```
"""
struct DQConversion <: M.AbstractDimConversion
    quantity::M.Observable{Any}
    automatic_units::Bool
    units_in_label::M.Observable{Bool}
end

function DQConversion(quantity=M.automatic; units_in_label=true)
    return DQConversion(quantity, quantity isa M.Automatic, units_in_label)
end

M.needs_tick_update_observable(conversion::DQConversion) = conversion.quantity

function M.get_ticks(conversion::DQConversion, ticks, scale, formatter, vmin, vmax)
    quantity = conversion.quantity[]
    quantity isa M.Automatic && return [], []
    unit_str = unit_string(quantity)
    tick_vals, labels = M.get_ticks(ticks, scale, formatter, vmin, vmax)
    if conversion.units_in_label[]
        labels = labels .* unit_str
    end
    return tick_vals, labels
end

function M.convert_dim_observable(conversion::DQConversion, value_obs::M.Observable, deregister)
    # TODO: replace with update_extrema
    if conversion.automatic_units
        conversion.quantity[] = oneunit(value_obs[][1])
    end
    result = map(conversion.quantity, value_obs; ignore_equal_values=true) do unit, values
        if !isempty(values)
            # try if conversion works, to through error if not!
            # Is there a function for this to check in DynamicQuantities?
            unit_convert(unit, values[1])
        end
        return M.convert_dim_value(conversion, values)
    end
    append!(deregister, result.inputs)
    return result
end

function M.convert_dim_value(conversion::DQConversion, values)
    return unit_convert(conversion.quantity[], values)
end

@testitem "1 arg expansion" begin
    using DynamicQuantities, Makie, Dates

    f, ax, pl = scatter(u"m" .* (1:10))
    @test pl isa Scatter{Tuple{Vector{Point2{Float64}}}}
end

@testitem "recipe" begin
    using DynamicQuantities, Makie, Dates
    const DQConversion = Base.get_extension(DynamicQuantities, :DynamicQuantitiesMakieExt).DQConversion

    @recipe(DQPlot, x) do scene
        return Attributes()
    end

    function Makie.plot!(plot::DQPlot)
        return scatter!(plot, plot.x, map(x -> x .* u"s", plot.x))
    end

    f, ax, pl = dqplot(1:5)

    pl_conversion = Makie.get_conversions(pl)
    ax_conversion = Makie.get_conversions(ax)

    @test pl_conversion[2] isa DQConversion
    @test ax_conversion[2] isa DQConversion
    @test pl.plots[1][1][] == Point{2,Float32}.(1:5, 1:5)
end

@testitem "unit switching" begin
    using DynamicQuantities, Makie
    f, ax, pl = scatter((1:10)u"m")
    @test_throws DynamicQuantities.DimensionError scatter!(ax, (1:10)u"kg")
    @test_throws MethodError scatter!(ax, (1:10))
end

@testitem "observables cleanup" begin
    using DynamicQuantities, Makie

    function test_cleanup(arg)
        obs = Observable(arg)
        f, ax, pl = scatter(obs)
        @test length(obs.listeners) == 1
        delete!(ax, pl)
        @test length(obs.listeners) == 0
    end

     test_cleanup([0.01u"km", 0.02u"km", 0.03u"km", 0.04u"km"])
end

# TODO: Move upstream to Makie.jl
@testitem "reftest" begin
    using DynamicQuantities, Makie
    const DQConversion = Base.get_extension(DynamicQuantities, :DynamicQuantitiesMakieExt).DQConversion

    fig = Figure()

    ax1 = Axis(fig[1, 1]; dim2_conversion=DQConversion(us"J/s"))
    ax2 = Axis(fig[1, 2]; dim2_conversion=DQConversion(us"mm/m^2"))
    ax3 = Axis(fig[2, 1]; dim1_conversion=DQConversion(us"W/m^2"), dim2_conversion=DQConversion(us"μm"))
    ax4 = Axis(fig[2, 2]; dim1_conversion=DQConversion(us"W/m^2"))

    scatter!(ax1, (1:10) .* u"J/s")
    scatter!(ax2, (1:10) .* u"K", exp.(1:10) .* u"mm/m^2")
    scatter!(ax3, 10 .^ (1:6) .* u"W/m^2", (1:6) .* 1000 .* u"nm")
    scatter!(ax4, (0:10) .* u"W/m^2", (0:10) .* u"g")
    scatter!(ax4, (0:10) .* u"kW/m^2", (0:10) .* u"kg")
end

end
