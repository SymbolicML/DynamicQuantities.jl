module DynamicQuantitiesScientificTypesExt

import DynamicQuantities: AbstractUnionQuantity, ustrip
import ScientificTypes as ST
import ScientificTypes.ScientificTypesBase as STB

STB.scitype(x::AbstractUnionQuantity, C::ST.DefaultConvention) = STB.scitype(ustrip(x), C)
STB.Scitype(::Type{<:AbstractUnionQuantity{T}}, C::ST.DefaultConvention) where {T} = STB.Scitype(T, C)

end
