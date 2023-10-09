using DynamicQuantities
using Aqua

Aqua.test_all(
    DynamicQuantities;
    project_toml_formatting=false,
    # Requires is only loaded on older Julia versions that do not support extensions
    stale_deps=(ignore=isdefined(Base, :get_extension) ? [:Requires] : Symbol[],),
)
