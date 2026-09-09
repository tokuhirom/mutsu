# Placeholder block parameters are readonly

Placeholder block parameters such as `$^x` now reject assignments with the
same readonly-variable error as ordinary non-`rw` scalar parameters. The mark
is applied at each call site, including map, grep, and first fast loops, so it is
scoped to the invocation and does not leak into later bindings.

Resolves Section A6 of #7556.
