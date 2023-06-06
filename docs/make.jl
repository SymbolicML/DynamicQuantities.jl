using DynamicUnits
using Documenter

DocMeta.setdocmeta!(DynamicUnits, :DocTestSetup, :(using DynamicUnits); recursive=true)

makedocs(;
    modules=[DynamicUnits],
    authors="MilesCranmer <miles.cranmer@gmail.com> and contributors",
    repo="https://github.com/MilesCranmer/DynamicUnits.jl/blob/{commit}{path}#{line}",
    sitename="DynamicUnits.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://MilesCranmer.github.io/DynamicUnits.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/MilesCranmer/DynamicUnits.jl",
    devbranch="main",
)
