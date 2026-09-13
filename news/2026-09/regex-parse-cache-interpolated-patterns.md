The regex parse cache now avoids allocating a formatted package-and-pattern
key for static patterns, scans static-pattern candidates without a temporary
character vector, and caches top-level regex parses after runtime interpolation.
Repeated matches with the same interpolated value therefore reuse the parsed
pattern, while patterns that consult ambient parse state remain uncached.
