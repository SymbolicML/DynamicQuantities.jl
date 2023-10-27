module DynamicQuantitiesRecursiveArrayToolsExt

import DynamicQuantities: AbstractQuantity
import RecursiveArrayTools: RecursiveArrayTools as RAT

function RAT.recursive_unitless_bottom_eltype(::Type{Q}) where {T,Q<:AbstractQuantity{T}}
    return T
end
function RAT.recursive_unitless_eltype(::Type{Q}) where {T,Q<:AbstractQuantity{T}}
    return T
end

end
