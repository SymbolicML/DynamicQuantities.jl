module DynamicQuantitiesScientificTypesExt

import DynamicQuantities: AbstractQuantity, ustrip
import ScientificTypes as ST
import ScientificTypesBase as STB

STB.scitype(x::AbstractQuantity, C::ST.DefaultConvention) = STB.scitype(ustrip(x), C)
STB.Scitype(::Type{<:AbstractQuantity{T}}, C::ST.DefaultConvention) where {T} = STB.Scitype(T, C)

end
