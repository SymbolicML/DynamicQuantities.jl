"""
    WriteOnceReadMany{V}(container::V)

A wrapper type for container that only defines methods
for appending to and reading to, but not modifying the container.

This is so that we can safely define a `@register_unit` interface
without needing to worry about the user overwriting previously
defined units and voiding the indexing of symbolic dimensions.
"""
struct WriteOnceReadMany{V}
    _raw_data::V

    WriteOnceReadMany(_raw_data) = new{typeof(_raw_data)}(_raw_data)
    WriteOnceReadMany{T}() where T = WriteOnceReadMany(T())
end

# Utility functions
Base.length(w::WriteOnceReadMany) = length(w._raw_data)
Base.lastindex(w::WriteOnceReadMany) = lastindex(w._raw_data)
Base.findfirst(val::Function, w::WriteOnceReadMany) = findfirst(val, w._raw_data)
Base.filter(val::Function, w::WriteOnceReadMany) = filter(val, w._raw_data)
Base.getindex(w::WriteOnceReadMany, i::Union{Integer,Symbol}) = getindex(w._raw_data, i)
Base.get(w::WriteOnceReadMany{<:AbstractDict}, a, b) = get(w._raw_data, a, b)

# Only define setindex! for Dicts, and throw an error if the key already exists
function Base.setindex!(w::WriteOnceReadMany{<:AbstractDict}, i, s::Symbol)
    haskey(w._raw_data, s) && error("Unit $s already exists at index $(w[s])")
    setindex!(w._raw_data, i, s)
    return w
end

Base.iterate(w::WriteOnceReadMany) = iterate(w._raw_data)
Base.iterate(w::WriteOnceReadMany, i) = iterate(w._raw_data, i)

Base.push!(w::WriteOnceReadMany, val...) = (push!(w._raw_data, val...); w)

