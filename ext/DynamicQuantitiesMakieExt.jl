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
    unit_convert.(Ref(quantity), x)
end

function unit_convert(quantity::UnionAbstractQuantity, value)
    conv = value / dimension(quantity)
    return Float64(ustrip(conv))
end

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
    return tick_vals, labels .* unit_str
end

function M.convert_dim_observable(conversion::DQConversion, value_obs::M.Observable, deregister)
    conversion.quantity[] = value_obs[][1]
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

@testitem "conversion" begin
    using DynamicQuantities, Makie, Dates
    const DQConversion = Base.get_extension(DynamicQuantities, :DynamicQuantitiesMakieExt).DQConversion

    f, ax, pl = scatter(u"m" .* (1:10))
    @test pl isa Scatter{Tuple{Vector{Point2{Float64}}}}

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

end
