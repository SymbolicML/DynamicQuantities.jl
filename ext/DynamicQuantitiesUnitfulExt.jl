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
const UNITFUL_EQUIVALENCIES = let basic = (length=u"m", mass=u"kg", time=u"s", current=u"A", temperature=u"K", luminosity=u"cd", amount=u"mol")
    NamedTuple((k => Unitful.upreferred(basic[k]) for k in keys(basic)))
end

Base.convert(::Type{Unitful.Quantity}, x::DynamicQuantities.Quantity) =
    let
        cumulator = DynamicQuantities.ustrip(x)
        dims = DynamicQuantities.dimension(x)
        for dim in keys(dims)
            value = dims[dim]
            iszero(value) && continue
            cumulator *= UNITFUL_EQUIVALENCIES[dim]^value
        end
        cumulator
    end

Base.convert(::Type{DynamicQuantities.Quantity}, x::Unitful.Quantity) =
    let
        value = Unitful.ustrip(Unitful.upreferred(x))
        dimension = convert(DynamicQuantities.Dimensions, Unitful.dimension(x))
        return DynamicQuantities.Quantity(value, dimension)
    end

Base.convert(::Type{DynamicQuantities.Dimensions}, d::Unitful.Dimensions{D}) where {D} =
    let
        cumulator = DynamicQuantities.Dimensions()
        for dim in D
            dim_symbol = _map_dim_name_to_dynamic_units(typeof(dim))
            dim_power = dim.power
            cumulator *= DynamicQuantities.Dimensions(; dim_symbol => dim_power)
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