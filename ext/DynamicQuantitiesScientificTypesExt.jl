module DynamicQuantitiesScientificTypesExt

if isdefined(Base, :get_extension)
    import DynamicQuantities: AbstractQuantity, ustrip
    import ScientificTypes as ST
    import ScientificTypes.ScientificTypesBase as STB
else
    import ..DynamicQuantities: AbstractQuantity, ustrip
    import ..ScientificTypes as ST
    import ..ScientificTypes.ScientificTypesBase as STB
end

STB.scitype(x::AbstractQuantity, C::ST.DefaultConvention) = STB.scitype(ustrip(x), C)
STB.Scitype(::Type{<:AbstractQuantity{T}}, C::ST.DefaultConvention) where {T} = STB.Scitype(T, C)

end
