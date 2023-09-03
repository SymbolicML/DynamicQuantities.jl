using BenchmarkTools
using DynamicQuantities

const SUITE = BenchmarkGroup()

SUITE["creation"] = let s = BenchmarkGroup()
    s["Quantity(x)"] = @benchmarkable Quantity(x) setup = (x = randn()) evals = 1000
    s["Quantity(x, length=y)"] = @benchmarkable Quantity(x, length=y) setup = (x = randn(); y = rand(1:5)) evals = 1000
    s
end

default() = Quantity(rand(), length=rand(1:5), mass=rand(1:5) // 2)

SUITE["with_numbers"] = let s = BenchmarkGroup()
    f1(x, i) = x^i
    s["^int"] = @benchmarkable $f1(x, i) setup = (x = default(); i = rand(1:5)) evals = 1000
    f2(x, y) = x * y
    s["*real"] = @benchmarkable $f2(x, y) setup = (x = default(); y = randn()) evals = 1000
    f3(x, i, y) = x^i * y
    s["^int * real"] = @benchmarkable $f3(x, i, y) setup = (x = default(); i = rand(1:5); y = randn()) evals = 1000
    s
end

SUITE["with_self"] = let s = BenchmarkGroup()
    f4(x) = inv(x)
    s["inv"] = @benchmarkable $f4(x) setup = (x = default()) evals = 1000
    f7(x) = ustrip(x)
    s["ustrip"] = @benchmarkable $f7(x) setup = (x = default()) evals = 1000
    f8(x) = dimension(x)
    s["dimension"] = @benchmarkable $f8(x) setup = (x = default()) evals = 1000
    s
end

SUITE["with_quantity"] = let s = BenchmarkGroup()
    f5(x, y) = x / y
    s["/y"] = @benchmarkable $f5(x, y) setup = (x = default(); y = default()) evals = 1000
    f6(x, y) = x + y
    s["+y"] = @benchmarkable $f6(x, y) setup = (x = default(); y = x + rand() * x) evals = 1000
    s
end

SUITE["with_array"] = let s = BenchmarkGroup()
    N = 10000
    f9(x) = x^2
    s["arr.^2"] = @benchmarkable $f9.(arr) setup = (arr = randn($N)) evals = 1000 # baseline
    s["qarr.^2"] = @benchmarkable $f9.(arr) setup = (arr = QuantityArray(randn($N), u"km/s")) evals = 1000
    s["arrq.^2"] = @benchmarkable $f9.(arr) setup = (arr = randn($N) .* u"km/s") evals = 1000
    f10(x) = x^4
    s["arr.^4"] = @benchmarkable $f9.(arr) setup = (arr = randn($N)) evals = 1000 # baseline
    s["qarr.^4"] = @benchmarkable $f9.(arr) setup = (arr = QuantityArray(randn($N), u"km/s")) evals = 1000
    s["arrq.^4"] = @benchmarkable $f9.(arr) setup = (arr = randn($N) .* u"km/s") evals = 1000
    s
end
