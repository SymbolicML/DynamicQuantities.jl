abstract type AbstractStaticDimensions{R,D,dim} <: AbstractDimensions{R} end

"""
    StaticDimensions{R,D<:AbstractDimensions{R},dim} <: AbstractStaticDimensions{R,D,dim}

Experiment to store the dimensions as a type parameter, so that one
can have Unitful-like behavior within DynamicQuantities.

This is not yet stable, so this type is not exported.
"""
struct StaticDimensions{R,D<:AbstractDimensions{R},dim} <: AbstractStaticDimensions{R,D,dim}

    StaticDimensions(d::AbstractDimensions) = new{eltype(d),typeof(d),d}()
    StaticDimensions{_R}(d::AbstractDimensions) where {_R} = (d = convert(with_type_parameters(typeof(d), _R), d); StaticDimensions(d))
    StaticDimensions{_R,_D}(d::AbstractDimensions) where {_R,_D} = (d = convert(_D, d); StaticDimensions(d))
    StaticDimensions(; kws...) = StaticDimensions(Dimensions(; kws...))
end

Base.propertynames(::AbstractStaticDimensions{R,D,dim}) where {R,D,dim} = propertynames(dim)
Base.getproperty(::AbstractStaticDimensions{R,D,dim}, k::Symbol) where {R,D,dim} = getproperty(dim, k)
Base.getindex(::AbstractStaticDimensions{R,D,dim}, i::Symbol) where {R,D,dim} = getindex(dim, i)

raw_dimension(::Type{<:AbstractStaticDimensions{R,D,dim}}) where {R,D,dim} = dim
dimension_names(::Type{<:AbstractStaticDimensions{R,D}}) where {R,D} = dimension_names(D)

constructorof(::Type{<:StaticDimensions}) = StaticDimensions
with_type_parameters(::Type{StaticDimensions{Rold,D}}, ::Type{R}) where {Rold,D,R}  = StaticDimensions{R,D}

function Base.promote_rule(::Type{<:StaticDimensions{R1,D1}}, ::Type{<:StaticDimensions{R2,D2}}) where {R1,D1,R2,D2}
    D = promote_type(D1, D2)
    return StaticDimensions{eltype(D),D}
end
function Base.promote_rule(::Type{<:StaticDimensions{R1,D1,dim1}}, ::Type{<:StaticDimensions{R2,D2,dim2}}) where {R1,D1,dim1,R2,D2,dim2}
    D = promote_type(D1, D2)
    R = eltype(D)
    dim1 == dim2 ? StaticDimensions{R,D,convert(D, dim1)} : StaticDimensions{R,D}
end
function Base.promote_rule(::Type{StaticDimensions{R1,D1,dim1}}, ::Type{D2}) where {R1,D1,dim1,D2<:Dimensions}
    return promote_type(D1, D2)
end
function Base.promote_rule(::Type{D1}, ::Type{<:StaticDimensions{R2,D2,dim2}}) where {D1<:AbstractDimensions,R2,D2,dim2}
    return promote_type(D1, D2)
end

function Base.convert(::Type{Q1}, q::Q2) where {
    T1,D1,Q1<:AbstractQuantity{T1,D1},
    dims,
    Q2<:AbstractQuantity{<:Any,<:AbstractStaticDimensions{<:Any,<:Any,dims}}
}
    val = ustrip(q)
    # First, construct dynamic version in the input types
    q_dynamic = new_quantity(Q2, val, dims)
    # Then, convert that
    return convert(Q1, q_dynamic)
end

map_dimensions(::F, args::AbstractStaticDimensions...) where {F<:Function} = error("not implemented.")

Base.:(==)(::AbstractStaticDimensions{<:Any,<:Any,dim1}, ::AbstractStaticDimensions{<:Any,<:Any,dim2}) where {dim1,dim2} = dim1 == dim2
@generated function map_dimensions(f::F, args::AbstractStaticDimensions...) where {F<:Function}
    cons = constructorof(promote_type(args...))
    dims = map(raw_dimension, args)
    return :($(cons)(map_dimensions(f, $(dims...))))
end
@generated function all_dimensions(f::F, args::AbstractStaticDimensions...) where {F<:Function}
    cons = constructorof(promote_type(args...))
    dims = map(raw_dimension, args)
    return :($(cons)(map_dimensions(f, $(dims...))))
end





################################################################################
# Tests ########################################################################
################################################################################
@testitem "Static dimensions basics" begin
    using DynamicQuantities
    using DynamicQuantities: StaticDimensions

    x = Quantity(1.0, StaticDimensions(length=1))
    d = Dimensions(length=1)
    @test typeof(x) === Quantity{Float64, StaticDimensions{eltype(d),typeof(d),d}}
    @test sprint(show, x) == "1.0 m"

    y = Quantity(1.0, StaticDimensions(time=-1))
    @test sprint(show, y) == "1.0 s⁻¹"

    # Only promotes to concrete type if dimensions equal
    @test promote_type(typeof(x), typeof(x)) === Quantity{Float64, StaticDimensions{eltype(d),typeof(d),d}}

    # Otherwise, is a union:
    @test promote_type(typeof(x), typeof(y)) === Quantity{Float64, StaticDimensions{eltype(d),typeof(d)}}
end

@testitem "Static dimensions math" begin
    using DynamicQuantities
    using DynamicQuantities: StaticDimensions

    x = Quantity(1.0, StaticDimensions(length=1))
    y = Quantity(2.0, StaticDimensions(time=-1))

    z = x * y
    @test sprint(show, z) == "2.0 m s⁻¹"
    @test z isa Quantity{Float64, <:StaticDimensions}
    @test z == Quantity(2.0, StaticDimensions(length=1, time=-1))

    z2 = x / y
    @test sprint(show, z2) == "0.5 m s"
    @test z2 isa Quantity{Float64, <:StaticDimensions}
    @test z2 == Quantity(0.5, StaticDimensions(length=1, time=1))

    # Check if inference works
    @inferred x * y
    @inferred x / y
end

@testitem "Conversion" begin
    using DynamicQuantities
    using DynamicQuantities: StaticDimensions

    x = 1.0u"m"
    y = convert(Quantity{Float64,StaticDimensions}, x)
    @test y isa Quantity{Float64,<:StaticDimensions}
    @test sprint(show, y) == "1.0 m"
    # @show typeof(y)
    @test dimension(x) isa Dimensions
    @test dimension(y) isa StaticDimensions

    # Should be able to convert back too:
    x2 = convert(Quantity{Float64,Dimensions}, y)
    @test x2 isa Quantity{Float64,<:Dimensions}
    @test x == x2

    # Should automatically convert:
    @test x == y
end

@testitem "Static dimensions arrays" begin
    using DynamicQuantities
    using DynamicQuantities: StaticDimensions

    x = [1.0u"m", 1.0u"km", 10u"Constants.Mpc"]
    x = Quantity{Float64,StaticDimensions}.(x)

    # The array should automatically promote to length
    d = dimension(1.0u"m")
    @test eltype(x) == Quantity{Float64,StaticDimensions{eltype(d),typeof(d),d}}

    # Should be able to do vectorized operations:
    x2 = x .^ 2
    d2 = dimension(1.0u"m^2")
    @test eltype(x2) == Quantity{Float64,StaticDimensions{eltype(d2),typeof(d2),d2}}
end
################################################################################

