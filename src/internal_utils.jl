"""
This file contains utility functions that are not specific to the
library, but are used throughout.
"""

@generated function fieldnames_equal(::Type{T1}, ::Type{T2}) where {T1,T2}
    # Needs to be a generated function to ensure hardcoded
    return static_fieldnames(T1) == static_fieldnames(T2)
end

const SUPERSCRIPT_MAPPING = ('⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹')
const INTCHARS = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

function to_superscript(s::AbstractString)
    chars = map(replace(s, "//" => "ᐟ")) do c
        if c ∈ INTCHARS
            SUPERSCRIPT_MAPPING[parse(Int, c)+1]
        elseif c == '-'
            '⁻'
        else
            c
        end
    end
    return join(chars)
end
