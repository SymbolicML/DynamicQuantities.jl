using DynamicQuantities
using Aqua

Aqua.test_all(DynamicQuantities; ambiguities=false, stale_deps=VERSION < v"1.11.0-")

# Only test range of versions so we don't go insane with ambiguities
@static if VERSION >= v"1.10.0-" && VERSION < v"1.11.0-"
    Aqua.test_ambiguities(DynamicQuantities)
end
