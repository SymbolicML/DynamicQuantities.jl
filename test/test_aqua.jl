using DynamicQuantities
using Aqua

Aqua.test_all(DynamicQuantities; ambiguities=false)

# Only test range of versions so we don't go insane with ambiguities
@static if VERSION >= v"1.10" && VERSION < v"1.11"
    Aqua.test_ambiguities(DynamicQuantities)
end
