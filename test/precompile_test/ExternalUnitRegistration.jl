module ExternalUnitRegistration

using DynamicQuantities: @register_unit, @u_str, @us_str
using DynamicQuantities: ALL_MAPPING, ALL_SYMBOLS,  DEFAULT_QUANTITY_TYPE
using DynamicQuantities: DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE, UNIT_SYMBOLS, UNIT_MAPPING
using Test

@register_unit Wb u"m^2*kg*s^-2*A^-1"

@testset "Register Unit Inside a Module" begin
    for collection in (UNIT_SYMBOLS, ALL_SYMBOLS, keys(ALL_MAPPING._raw_data), keys(UNIT_MAPPING._raw_data))
        @test :Wb ∈ collection
    end

    w = u"Wb"
    ws = us"Wb"
    @test w isa DEFAULT_QUANTITY_TYPE
    @test ws isa DEFAULT_SYMBOLIC_QUANTITY_OUTPUT_TYPE
end

end
