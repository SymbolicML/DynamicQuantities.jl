const Dimensionless = (𝐋=0 // 1, 𝐌=0 // 1, 𝐓=0 // 1, 𝐈=0 // 1, 𝚯=0 // 1, 𝐉=0 // 1, 𝐍=0 // 1)
const NumDimensions = length(Dimensionless)
const R = Rational{Int}
const DefaultDimensionType = typeof(Dimensionless)
const DefaultDataType = NTuple{NumDimensions,R}
const VALID_KEYS = (:𝐋, :𝐌, :𝐓, :𝐈, :𝚯, :𝐉, :𝐍)
const VALID_SYNONYMS = (:length, :mass, :time, :current, :temperature, :luminosity, :amount)
const SYNONYM_MAPPING = NamedTuple(VALID_SYNONYMS .=> VALID_KEYS)
const VALID_KWARGS = Tuple(union(VALID_KEYS, VALID_SYNONYMS))

struct Dimensions
    data::DefaultDimensionType

    Dimensions(data::DefaultDimensionType) = new(data)
    Dimensions(data::DefaultDataType) = Dimensions(DefaultDimensionType(data))
    Dimensions(kws::NamedTuple) =
        let
            foreach(keys(kws)) do k
                @assert (k in VALID_KWARGS) "Invalid dimension: $k. Valid choices are $VALID_KWARGS."
            end
            new((
                𝐋=tryrationalize(Int, get(kws, :𝐋, get(kws, :length, 0 // 1))),
                𝐌=tryrationalize(Int, get(kws, :𝐌, get(kws, :mass, 0 // 1))),
                𝐓=tryrationalize(Int, get(kws, :𝐓, get(kws, :time, 0 // 1))),
                𝐈=tryrationalize(Int, get(kws, :𝐈, get(kws, :current, 0 // 1))),
                𝚯=tryrationalize(Int, get(kws, :𝚯, get(kws, :temperature, 0 // 1))),
                𝐉=tryrationalize(Int, get(kws, :𝐉, get(kws, :luminosity, 0 // 1))),
                𝐍=tryrationalize(Int, get(kws, :𝐍, get(kws, :amount, 0 // 1)))
            ))
        end
    Dimensions(; kws...) = isempty(kws) ? new(Dimensionless) : Dimensions(NamedTuple(kws))
end

struct Quantity{T}
    value::T
    dimensions::Dimensions
    valid::Bool

    Quantity(x; kws...) = new{typeof(x)}(x, Dimensions(; kws...), true)
    Quantity(x, valid::Bool; kws...) = new{typeof(x)}(x, Dimensions(; kws...), valid)
    Quantity(x, d::Dimensions) = new{typeof(x)}(x, d, true)
    Quantity(x, d::Dimensions, valid::Bool) = new{typeof(x)}(x, d, valid)
end
