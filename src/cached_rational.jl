import Base: divgcd

function _divgcd(x::Int8, y::Int8)
    iszero(y) && return (zero(Int8), zero(Int8))
    g = gcd(x, y)
    div(x, g), div(y, g)
end

const gcd_table = Tuple(Tuple(_divgcd(Int8(i), Int8(j)) for i = (typemin(Int8)+1):typemax(Int8)) for j = (typemin(Int8)+1):typemax(Int8));

function divgcd(x::Int8, y::Int8)
    gcd_table[x+2-typemin(Int8)][y+2-typemin(Int8)]
end
