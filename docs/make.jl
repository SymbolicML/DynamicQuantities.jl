using DynamicUnits
using Documenter

DocMeta.setdocmeta!(DynamicUnits, :DocTestSetup, :(using DynamicUnits); recursive=true)

readme = open(dirname(@__FILE__) * "/../README.md") do io
    read(io, String)
end

# We replace every instance of <img src="IMAGE" ...> with ![](IMAGE).
readme = replace(readme, r"<img src=\"([^\"]+)\"[^>]+>.*" => s"![](\1)")

# Then, we remove any line with "<div" on it:
readme = replace(readme, r"<[/]?div.*" => s"")

# Finally, we read in file docs/src/index_base.md:
index_base = open(dirname(@__FILE__) * "/src/index_base.md") do io
    read(io, String)
end

# And then we create "/src/index.md":
open(dirname(@__FILE__) * "/src/index.md", "w") do io
    write(io, readme)
    write(io, "\n")
    write(io, index_base)
end

makedocs(;
    modules=[DynamicUnits],
    authors="MilesCranmer <miles.cranmer@gmail.com> and contributors",
    repo="https://github.com/SymbolicML/DynamicUnits.jl/blob/{commit}{path}#{line}",
    sitename="DynamicUnits.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://symbolicml.org/DynamicUnits.jl",
        edit_link="main",
        assets=String[]
    ),
    pages=[
        "Home" => "index.md",
        "API" => "api.md",
    ]
)

deploydocs(;
    repo="github.com/SymbolicML/DynamicUnits.jl",
    devbranch="main"
)
