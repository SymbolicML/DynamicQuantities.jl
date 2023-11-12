module DynamicQuantitiesDiffEqBaseExt

using DynamicQuantities: 
    UnionAbstractQuantity, ustrip, QuantityArray

import DiffEqBase

DiffEqBase.value(x::UnionAbstractQuantity) = ustrip(x)
DiffEqBase.recursive_length(u::UnionAbstractQuantity) = recursive_length(ustrip(u))
DiffEqBase.recursive_length(u::QuantityArray) = recursive_length(ustrip(u))

@inline function DiffEqBase.UNITLESS_ABS2(x::UnionAbstractQuantity)
    abs2(ustrip(x))
end
function DiffEqBase.abs2_and_sum(x::UnionAbstractQuantity, y)
    reduce(Base.add_sum, ustrip(x), init = zero(real(DiffEqBase.value(x)))) +
        reduce(Base.add_sum, y, init = zero(real(DiffEqBase.value(eltype(y)))))
end

DiffEqBase.NAN_CHECK(x::UnionAbstractQuantity) = NAN_CHECK(ustrip(x))

end