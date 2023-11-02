struct WildcardDimensions{R} <: AbstractDimensions{R}
    # We simply choose Bool since it will get promoted
    WildcardDimensions() = new{Bool}()
end

constructorof(::Type{<:WildcardDimensions}) = WildcardDimensions
dimension_names(::Type{<:WildcardDimensions}) = ()

# Wildcard dimensions are always compatible:
Base.:(==)(::AbstractDimensions, ::WildcardDimensions) = true
Base.:(==)(::WildcardDimensions, ::AbstractDimensions) = true

# For any zero/oneunit specified on the type, we return a wildcard dimension instead:
Base.zero(::Type{Q}) where {T,Q<:UnionAbstractQuantity{T}} = constructorof(Q)(zero(T), WildcardDimensions())

# For propagation rules, we assume all relevant dimensions are zero
Base.getproperty(::WildcardDimensions, ::Symbol) = zero(Bool)
Base.iszero(::WildcardDimensions) = true

# Always promote to the other dimension type:
Base.promote_rule(::Type{D}, ::Type{<:WildcardDimensions}) where {D<:AbstractDimensions} = D

# Other utility rules:
Base.show(io::IO, ::WildcardDimensions) = print(io, "[*]")

# + and - will remove wildcard dimensions:
combine_dimension_with_wildcard(q1::UnionAbstractQuantity, q2::UnionAbstractQuantity) = combine_dimension_with_wildcard(dimension(q1), dimension(q2))
combine_dimension_with_wildcard(d::AbstractDimensions, ::AbstractDimensions) = d
combine_dimension_with_wildcard(::WildcardDimensions, d::AbstractDimensions) = d
