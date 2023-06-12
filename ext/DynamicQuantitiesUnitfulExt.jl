module DynamicQuantitiesUnitfulExt

if isdefined(Base, :get_extension)
    import DynamicQuantities
    import Unitful
    import Unitful: @u_str
else
    import ..DynamicQuantities
    import ..Unitful
    import ..Unitful: @u_str
end

# This lets the user override the preferred units:
function unitful_equivalences()
    si_units = (length=u"m", mass=u"kg", time=u"s", current=u"A", temperature=u"K", luminosity=u"cd", amount=u"mol")
    for k in keys(si_units)
        if Unitful.upreferred(si_units[k]) !== si_units[k]
            error("Found custom `Unitful.preferunits`. This is not supported when interfacing Unitful and DynamicQuantities: you must leave the default `Unitful.upreferred`, which is the SI base units.")
        end
    end
    return NamedTuple((k => si_units[k] for k in keys(si_units)))
end

Base.convert(::Type{Unitful.Quantity}, x::DynamicQuantities.Quantity) =
    let
        cumulator = DynamicQuantities.ustrip(x)
        dims = DynamicQuantities.dimension(x)
        equiv = unitful_equivalences()
        for dim in keys(dims)
            value = dims[dim]
            iszero(value) && continue
            cumulator *= equiv[dim]^value
        end
        cumulator
    end

Base.convert(::Type{DynamicQuantities.Quantity}, x::Unitful.Quantity{T}) where {T} = convert(DynamicQuantities.Quantity{T,DynamicQuantities.DEFAULT_DIM_TYPE}, x)
Base.convert(::Type{DynamicQuantities.Quantity{T,R}}, x::Unitful.Quantity) where {T,R} =
    let
        value = Unitful.ustrip(Unitful.upreferred(x))
        dimension = convert(DynamicQuantities.Dimensions{R}, Unitful.dimension(x))
        return DynamicQuantities.Quantity(convert(T, value), dimension)
    end

Base.convert(::Type{DynamicQuantities.Dimensions}, d::Unitful.Dimensions) = convert(DynamicQuantities.Dimensions{DynamicQuantities.DEFAULT_DIM_TYPE}, d)
Base.convert(::Type{DynamicQuantities.Dimensions{R}}, d::Unitful.Dimensions{D}) where {R,D} =
    let
        cumulator = DynamicQuantities.Dimensions{R}()
        for dim in D
            dim_symbol = _map_dim_name_to_dynamic_units(typeof(dim))
            dim_power = dim.power
            cumulator *= DynamicQuantities.Dimensions(R; dim_symbol => dim_power)
        end
        cumulator
    end

function _map_dim_name_to_dynamic_units(::Type{Unitful.Dimension{D}}) where {D}
    # (We could do this automatically, but it's more obvious what we are doing this way.)
    D == :Length && return :length
    D == :Mass && return :mass
    D == :Time && return :time
    D == :Current && return :current
    D == :Temperature && return :temperature
    D == :Luminosity && return :luminosity
    D == :Amount && return :amount
    error("Unknown dimension: $D")
end


end
