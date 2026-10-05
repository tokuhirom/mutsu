# Map view methods use handler rows

`Map.values`, `kv`, `pairs` and `antipairs` now dispatch through `Map` handler
rows, and `Hash` inherits those rows through its MRO. The cascade uses the
same handlers for fallback calls, preserving dereferenced values and original
typed keys in object hashes. The focused regression covers both `Map` and
`Hash` receivers.

A paired callgrind run used a 20,000-iteration loop over a two-entry Map,
calling all four views and reading each result's `.elems`. The second run fell
from 1,631,207,873 to 1,497,226,995 instructions (-8.21%). The baseline used
the same executable sources as current `main`; the intervening main commit
changed documentation only.
