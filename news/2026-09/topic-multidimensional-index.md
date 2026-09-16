# Preserve dimensions in topical multidimensional subscripts

Topical subscripts such as `.[2; .[0; 7]]` now retain their semicolon-separated
dimensions. Previously a legacy parser path flattened them into a one-dimensional
slice, causing valid nested collection writes to fail.
