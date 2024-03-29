for (type, true_base_type, _) in ABSTRACT_QUANTITY_TYPES
    base_type = true_base_type <: Number ? true_base_type : Number
    @eval begin
        function Base.complex(a::$type, b::$type)
            a, b = promote_except_value(a, b)
            dimension(a) == dimension(b) || throw(DimensionError(a, b))
            return new_quantity(typeof(a), complex(ustrip(a), ustrip(b)), dimension(a))
        end
        function Base.complex(a::$type, b::$base_type)
            iszero(dimension(a)) || throw(DimensionError(a, b))
            return new_quantity(typeof(a), complex(ustrip(a), b), dimension(a))
        end
        function Base.complex(a::$base_type, b::$type)
            iszero(dimension(b)) || throw(DimensionError(a, b))
            return new_quantity(typeof(b), complex(a, ustrip(b)), dimension(b))
        end
    end
    for (type2, _, _) in ABSTRACT_QUANTITY_TYPES
        type == type2 && continue
        @eval Base.complex(a::$type, b::$type2) = complex(promote_except_value(a, b)...)
    end
end
