module DynamicUnitsUnitfulExt

if isdefined(Base, :get_extension)
    import DynamicUnits
    import Unitful
    import Unitful: @u_str
else
    import ..DynamicUnits
    import ..Unitful
    import ..Unitful: @u_str
end

# This lets the user override the preferred units:
const UNITFUL_EQUIVALENCIES = let basic = (𝐋=u"m", 𝐌=u"kg", 𝐓=u"s", 𝐈=u"A", 𝚯=u"K", 𝐉=u"cd", 𝐍=u"mol")
    NamedTuple((k => Unitful.upreferred(basic[k]) for k in keys(basic)))
end

Base.convert(::Type{Unitful.Quantity}, x::DynamicUnits.Quantity) =
    let
        cumulator = DynamicUnits.ustrip(x)
        dims = DynamicUnits.dimension(x)
        for dim in keys(dims)
            value = dims[dim]
            iszero(value) && continue
            cumulator *= UNITFUL_EQUIVALENCIES[dim]^value
        end
        cumulator
    end

Base.convert(::Type{DynamicUnits.Quantity}, x::Unitful.Quantity) =
    let
        value = Unitful.ustrip(Unitful.upreferred(x))
        dimension = convert(DynamicUnits.Dimensions, Unitful.dimension(x))
        return DynamicUnits.Quantity(value, dimension)
    end

Base.convert(::Type{DynamicUnits.Dimensions}, d::Unitful.Dimensions{D}) where {D} =
    let
        cumulator = DynamicUnits.Dimensions()
        for dim in D
            dim_symbol = _map_dim_name_to_dynamic_units(typeof(dim))
            dim_power = dim.power
            cumulator *= DynamicUnits.Dimensions(; dim_symbol => dim_power)
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