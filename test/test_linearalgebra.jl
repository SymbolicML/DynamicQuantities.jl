# using DynamicQuantities, LinearAlgebra

# # check ldiv with 2D x 2D `QuantityArray`s
# D = A * C
# @test within(A \ D, C, 1e-12)

# F = svd(A)
# U, σ, V = F

# inv(F) # doesn't return a QuantityArray, could add a wrapper here

# @test isequal(dimension(σ), dimension(A))

# Σ = Diagonal(σ)
# @test within(U * Σ * V', A, 1e-10)

# # test that diagm runs
# Σwide = diagm(2, 3, σ)
# Σsquare = diagm(σ)

# # SVD in modal form
# # truncated SVD
# K = 2
# U[:, 1:K] * Diagonal(σ[1:K]) * V[:, 1:K]'

# K = 1
# U[:, 1:K] * Diagonal(σ[1:K]) * V[:, 1:K]'

# # retain QuantityArray, best way to store this info
# @test Σ * Σ isa QuantityArray
# @test U * Σ isa QuantityArray
# @test U' * Σ isa QuantityArray
# @test transpose(U) * Σ isa QuantityArray
# @test Σ * V isa QuantityArray
# @test Σ * V' isa QuantityArray
# @test Σ * transpose(V) isa QuantityArray
# @test U * Σ * V' isa QuantityArray

# F = eigen(A)
# vals, vecs = F
# @test isequal(dimension(vals), dimension(A))
# # inv(F) # doesn't work
# @test within(det(F), det(A), 1e-10)

# # eigenstructure satisfied?
# @test within(A * vecs, vecs * Diagonal(vals), 1e-10)

# det(A) # does it run?

# # svdvals(A) # error, LinearAlgebra.eigen.jl doesn't handle it

# # isapprox(ustrip(transpose(A)),transpose(ustrip(A))) # no show method for Transpose{QuantityArray}
