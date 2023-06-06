var documenterSearchIndex = {"docs":
[{"location":"index_base/","page":"DynamicUnits","title":"DynamicUnits","text":"CurrentModule = DynamicUnits","category":"page"},{"location":"index_base/#DynamicUnits","page":"DynamicUnits","title":"DynamicUnits","text":"","category":"section"},{"location":"index_base/","page":"DynamicUnits","title":"DynamicUnits","text":"Documentation for DynamicUnits.","category":"page"},{"location":"index_base/","page":"DynamicUnits","title":"DynamicUnits","text":"","category":"page"},{"location":"index_base/","page":"DynamicUnits","title":"DynamicUnits","text":"Modules = [DynamicUnits]","category":"page"},{"location":"#DynamicUnits","page":"Home","title":"DynamicUnits","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"(Image: Stable) (Image: Dev) (Image: Build Status) (Image: Coverage)","category":"page"},{"location":"","page":"Home","title":"Home","text":"This defines a simple statically-typed Quantity type for Julia. Physical dimensions are stored as a value, as opposed to a parametric type, as in Unitful.jl. This is done to allow for calculations where physical dimensions are not known at compile time.","category":"page"},{"location":"#Performance","page":"Home","title":"Performance","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"DynamicUnits can greatly outperform Unitful when the compiler cannot infer dimensions in a function:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> using BenchmarkTools, DynamicUnits; import Unitful\n\njulia> dyn_uni = Quantity(0.2, mass=1, length=0.5, amount=3)\n0.2 𝐋 ¹ᐟ² 𝐌 ¹ 𝐍 ³\n\njulia> unitful = convert(Unitful.Quantity, dyn_uni)\n0.2 kg m¹ᐟ² mol³\n\njulia> f(x) = x ^ rand(1:10) * 0.3;\n\njulia> @btime f($dyn_uni);\n  80.449 ns (0 allocations: 0 bytes)\n\njulia> @btime f($unitful);\n  29.666 μs (42 allocations: 1.91 KiB)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Here, the DynamicUnits Quantity allows the compiler to build a function that is type stable, while the Unitful Quantity requires type inference at runtime.","category":"page"},{"location":"","page":"Home","title":"Home","text":"However, if the dimensions in your function can be inferred by the compiler, then you are better off using Unitful:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> g(x) = x ^ 2 * 0.3;\n\njulia> @btime g($dyn_uni);\n  56.317 ns (0 allocations: 0 bytes)\n\njulia> @btime g($unitful);\n  1.958 ns (0 allocations: 0 bytes)","category":"page"},{"location":"","page":"Home","title":"Home","text":"While both of these are type stable, because Unitful parametrizes the type on the dimensions, functions can specialize to units and the compiler can optimize away units from the code.","category":"page"},{"location":"#Usage","page":"Home","title":"Usage","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"You can create a Quantity object with a value and keyword arguments for the powers of the physical dimensions (mass, length, time, current, temperature, luminosity, amount):","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> x = Quantity(0.3, mass=1, length=0.5)\n0.3 𝐋 ¹ᐟ² 𝐌 ¹\n\njulia> y = Quantity(10.2, mass=2, time=-2)\n10.2 𝐌 ² 𝐓 ⁻²","category":"page"},{"location":"","page":"Home","title":"Home","text":"Elementary calculations with +, -, *, /, ^, sqrt, cbrt are supported:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> x * y\n3.0599999999999996 𝐋 ¹ᐟ² 𝐌 ³ 𝐓 ⁻²\n\njulia> x / y\n0.029411764705882353 𝐋 ¹ᐟ² 𝐌 ⁻¹ 𝐓 ²\n\njulia> x ^ 3\n0.027 𝐋 ³ᐟ² 𝐌 ³\n\njulia> x ^ -1\n3.3333333333333335 𝐋 ⁻¹ᐟ² 𝐌 ⁻¹\n\njulia> sqrt(x)\n0.5477225575051661 𝐋 ¹ᐟ⁴ 𝐌 ¹ᐟ²\n\njulia> x ^ 1.5\n0.1643167672515498 𝐋 ³ᐟ⁴ 𝐌 ³ᐟ²","category":"page"},{"location":"","page":"Home","title":"Home","text":"Each of these values has the same type, thus obviating the need for type inference at runtime.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Furthermore, we can do dimensional analysis automatically:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> x + 3 * x\n1.2 𝐋 ¹ᐟ² 𝐌 ¹\n\njulia> x + y\nINVALID","category":"page"},{"location":"","page":"Home","title":"Home","text":"We can see the second one has valid(quantity) == false. This doesn't throw an error by default, as it allows for stable return values.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The dimensions of a Quantity can be accessed either with dimension(quantity) for the entire Dimensions object:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> dimension(x)\n𝐋 ¹ᐟ² 𝐌 ¹","category":"page"},{"location":"","page":"Home","title":"Home","text":"or with umass, ulength, etc., for the various dimensions:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> umass(x)\n1//1\n\njulia> ulength(x)\n1//2","category":"page"},{"location":"","page":"Home","title":"Home","text":"Finally, you can strip units with ustrip:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> ustrip(x)\n0.2","category":"page"},{"location":"#Units","page":"Home","title":"Units","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Despite the name, DynamicUnits does not actually work with units. Instead, it works with dimensions. You can use Unitful to parse units, and use the DynamicUnits->Unitful extension for conversion:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> using Unitful: Unitful, @u_str\n\njulia> x = 0.5u\"km/s\"\n0.5 km s⁻¹\n\njulia> y = convert(DynamicUnits.Quantity, x)\n500.0 𝐋 ¹ 𝐓 ⁻¹\n\njulia> y2 = y^2 * 0.3\n75000.0 𝐋 ² 𝐓 ⁻²\n\njulia> x2 = convert(Unitful.Quantity, y2)\n75000.0 m² s⁻²\n\njulia> x^2*0.3 == x2\ntrue","category":"page"},{"location":"#Vectors","page":"Home","title":"Vectors","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"There is not a separate class for vectors, but you can create units like so:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> randn(5) .* Dimensions(mass=2/5, length=2)\n5-element Vector{Quantity{Float64}}:\n -0.6450221578668845 𝐋 ² 𝐌 ²ᐟ⁵\n 0.4024829670050946 𝐋 ² 𝐌 ²ᐟ⁵\n 0.21478863605789672 𝐋 ² 𝐌 ²ᐟ⁵\n 0.0719774550969669 𝐋 ² 𝐌 ²ᐟ⁵\n -1.4231241943420674 𝐋 ² 𝐌 ²ᐟ⁵","category":"page"},{"location":"","page":"Home","title":"Home","text":"Because it is type stable, you can have mixed units in a vector too:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> v = [Quantity(randn(), mass=rand(0:5), length=rand(0:5)) for _=1:5]\n5-element Vector{Quantity{Float64}}:\n 2.2054411324716865 𝐌 ³\n -0.01603602425887379 𝐋 ⁴ 𝐌 ³\n 1.4388184352393647 \n 2.382303019892503 𝐋 ² 𝐌 ¹\n 0.6071392594021706 𝐋 ⁴ 𝐌 ⁴","category":"page"},{"location":"","page":"Home","title":"Home","text":"CurrentModule = DynamicUnits","category":"page"},{"location":"#DynamicUnits-2","page":"Home","title":"DynamicUnits","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Documentation for DynamicUnits.","category":"page"},{"location":"","page":"Home","title":"Home","text":"","category":"page"},{"location":"","page":"Home","title":"Home","text":"Modules = [DynamicUnits]","category":"page"}]
}
