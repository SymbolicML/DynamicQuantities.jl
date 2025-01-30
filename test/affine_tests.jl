using Revise
using Test 
using DynamicQuantities

DT = DynamicQuantities.DEFAULT_DIM_BASE_TYPE
kelvin  = AffineDimensions(scale=1.0, offset=0.0, basedim=u"K")
celsius = AffineDimensions(scale=1.0, offset=273.15, basedim=u"K")
fahrenheit = AffineDimensions(scale=5/9, offset=-(5/9)*32, basedim=celsius)

uconvert(Quantity(1.0, fahrenheit), Quantity(-40.0, celsius))
uconvert(us"K", Quantity(-40.0, celsius))

Quantity(-40.0, celsius) isa DynamicQuantities.AbstractQuantityOrArray{<:Any, <:AbstractAffineDimensions}

Quantity(1.0, kelvin)*Quantity(1.0, kelvin)

velocity = ua"mm/s"

@register_unit lb 0.453592u"kg"
mass_flow = ua"lb/min"
