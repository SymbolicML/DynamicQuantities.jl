#import Pkg; Pkg.activate(".")
#using Revise
using DynamicQuantities, LinearAlgebra

# checks for unit consistency with subtraction
within(A,B,tol) =  maximum(abs.(ustrip(A - B))) < tol

v = randn(2,2)
d = 1u"m"
e = u"m"
A = QuantityArray(v, d) # Create a `QuantityArray` with value `value` and dimensions `dimensions`.

# vector multiplication
q = [1,2]
r = A\q
@test isequal(dimension(A*r),dimension(QuantityArray(q)))
@test within(ustrip(A*r),q,1e-10)

# ldiv 2D QuantityArray and 1D QuantityArray
g = QuantityArray(randn(2), d) 
h = A\g
@test within(A*h,g,1e-10)

# test inv
B = inv(A)
@test isapprox(B*A,I(2),rtol = 1e-10)
@test isapprox(A*B,I(2),rtol = 1e-10)

vc = randn(2,2)
dc = 1u"kg"
C = QuantityArray(vc, dc) # Create a `QuantityArray` with v

# check ldiv with 2D x 2D `QuantityArray`s
D = A*C
@test within(A\D,C,1e-12)

F = svd(A)
U,σ,V = F

inv(F) # doesn't return a QuantityArray, could add a wrapper here

@test isequal(dimension(σ),dimension(A))

Σ = Diagonal(σ)
@test within(U*Σ*V',A,1e-10)

# SVD in modal form
# truncated SVD
K = 2
U[:,1:K]*Diagonal(σ[1:K])*V[:,1:K]'

K = 1
U[:,1:K]*Diagonal(σ[1:K])*V[:,1:K]'

# retain QuantityArray, best way to store this info
@test Σ*Σ isa QuantityArray
@test U*Σ isa QuantityArray
@test U'*Σ isa QuantityArray
@test transpose(U)*Σ isa QuantityArray
@test Σ*V isa QuantityArray
@test Σ*V' isa QuantityArray
@test Σ*transpose(V) isa QuantityArray
@test U*Σ*V' isa QuantityArray

F = eigen(A)
vals,vecs = F
@test isequal(dimension(vals),dimension(A))
# inv(F) # doesn't work
@test within(det(F),det(A),1e-10)

# eigenstructure satisfied?
@test within(A*vecs,vecs*Diagonal(vals),1e-10)

det(A) # does it run?

# svdvals(A) # error, LinearAlgebra.eigen.jl doesn't handle it

# isapprox(ustrip(transpose(A)),transpose(ustrip(A))) # no show method for Transpose{QuantityArray}
