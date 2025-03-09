module DynamicQuantitiesMakieExt

using DynamicQuantities: UnionAbstractQuantity, ustrip, dimension

import Makie as M

struct DQConversion <: M.AbstractDimConversion
    quantities::M.Observable{Any}
    DQConversion() = new(M.automatic)
end

M.expand_dimensions(::M.PointBased, y::AbstractVector{<:UnionAbstractQuantity}) = (keys(y), y)

M.needs_tick_update_observable(conversion::DQConversion) = nothing

M.create_dim_conversion(::Type{<:UnionAbstractQuantity}) = DQConversion()

M.MakieCore.should_dim_convert(::Type{<:UnionAbstractQuantity}) = true

M.convert_dim_value(::DQConversion, quantities) = [ustrip(q) for q in quantities]

function M.convert_dim_observable(conversion::DQConversion, values_obs::M.Observable, deregister)
    result = M.Observable(Float64[])
    f = M.on(values_obs; update=true) do values
        result[] = M.convert_dim_value(conversion, values)
        conversion.quantities[] = values
    end
    push!(deregister, f)
    return result
end

function M.get_ticks(conversion::DQConversion, ticks, scale, formatter, vmin, vmax)
    tick_vals, labels = M.get_ticks(ticks, scale, formatter, vmin, vmax)
    return tick_vals, string.(labels, string(dimension(conversion.quantities[])))
end

end
