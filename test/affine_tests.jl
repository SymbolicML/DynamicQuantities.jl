using Revise
using Test 
using DynamicQuantities

DT = DynamicQuantities.DEFAULT_DIM_BASE_TYPE
kelvin  = AffineDimensions(scale=1.0, offset=0.0, basedim=u"K")
rankine = AffineDimensions(scale=5/9, offset=0.0, basedim=kelvin)
fahrenheit = AffineDimensions(scale=1.0, offset=459.67, basedim=rankine)
celsius = AffineDimensions(scale=9/5, offset=32, basedim=fahrenheit)


uconvert(Quantity(1.0, fahrenheit), Quantity(-40.0, celsius))
uconvert(Quantity(1.0, celsius), Quantity(-40.0, fahrenheit))
uconvert(us"K", Quantity(-40.0, celsius))

Quantity(-40.0, celsius) isa DynamicQuantities.AbstractQuantityOrArray{<:Any, <:AbstractAffineDimensions}

Quantity(1.0, kelvin)*Quantity(1.0, kelvin)

velocity = ua"mm/s"

@register_unit lb 0.453592u"kg"
mass_flow = ua"lb/min"

@register_unit psi 6.89476us"kPa"
@register_affine_unit psig AffineDimensions(offset=u"Constants.atm", basedim=u"psi")
uconvert(ua"psig", u"Constants.atm")
uexpand(0ua"psig")

@register_unit half_meter 0.5u"m"
AffineDimensions(offset=1, basedim=0.5u"m")
AffineDimensions(offset=1, basedim=u"half_meter")

uconvert(ua"°C", 0ua"°F")
uconvert(ua"°F", 0ua"°C")
uexpand(0ua"°F")
uconvert(ua"°C", 0u"K")
uconvert(ua"°C", -40ua"°F")
uconvert(ua"°F", -40ua"°C")