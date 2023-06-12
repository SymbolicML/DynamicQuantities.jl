module linsolve


using SparseArrays,Sparspak,DynamicQuantities, LinearAlgebra
using GenericLinearAlgebra

function makeproblem(n;dimA=DynamicQuantities.Dimensions(length=1),dimb=DynamicQuantities.Dimensions(tim=1))
    A=-sprand(n,n,0.5)+100I
    SparseMatrixCSC(size(A)...,A.colptr,A.rowval,Quantity.(A.nzval,dimA)), Quantity.(rand(n),dimb)
    
#    [ Quantity(b0[i],length=1) for i=1:n]
    #DynamicQuantities.Quantity.(rand(10),time=1)
end


function densetest(n)
    As,b=makeproblem(n)
    A=Matrix(As)
    lu(A)\b
end

function sparsetest(n)
    A,b=makeproblem(n)
    sparspaklu(A)\b
end

end
