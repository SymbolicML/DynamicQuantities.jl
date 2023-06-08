import Base: divgcd

const gcd_table =
    Tuple(
        Tuple(
            if j == 0
                Rational{Int8}.((0, 0))
            else
                Rational{Int8}.(divgcd(Int16(i), Int16(j)))
            end
            for i = (typemin(Int8)+1):typemax(Int8)
        )
        for j = (typemin(Int8)+1):typemax(Int8)
    );

# Pirate divgcd to speed it up for Int8
function divgcd(x::Int8, y::Int8)
    @inbounds gcd_table[y+128][x+128]
end
