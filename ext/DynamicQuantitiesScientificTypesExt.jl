module DynamicQuantitiesScientificTypesExt

if isdefined(Base, :get_extension)
    import DynamicQuantities: AbstractQuantity, ustrip
    import ScientificTypes
    import ScientificTypes: scitype
else
    import ..DynamicQuantities: AbstractQuantity, ustrip
    import ..ScientificTypes
    import ..ScientificTypes: scitype
end

scitype(x::AbstractQuantity) = scitype(ustrip(x))
scitype(x::AbstractArray{<:AbstractQuantity}) = scitype(ustrip.(x))

end
