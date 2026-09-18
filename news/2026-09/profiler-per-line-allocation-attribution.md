# The profiler can attribute allocations to source lines

An `alloc-stats` build can now report exact allocation counts and requested
bytes for each Raku source line in a profile. Enable it with
`MUTSU_ALLOC_STATS=1` together with `--profile`; the JSON document exposes an
`allocations` object on measured line rows and marks the document with
`header.allocation_stats`.

Because the counting allocator changes allocation timing, this mode intentionally
omits sampled time. The normal build and ordinary profiles are unchanged.
