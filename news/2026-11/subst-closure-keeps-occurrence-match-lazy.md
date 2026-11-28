# A `.subst` closure replacement no longer materializes every occurrence's Match

`.subst(rx, { 'u' ~ $0 }, :g)` calls its block once per occurrence with that
occurrence's Match as `$/` and as the block's argument. The Match is lazy
(ADR-0016), but something on the way into the block always forced its
Instance-shaped attribute map. Tracing every forcing site with a breakpoint on
the materialization counter found eight. Each one used `view()` to ask a
question a tag probe answers: "is this a Pair?", "is this a Junction?", "is
this a ContainerRef?". A lazy Match presents as an Instance under `view()`,
so it was built just to answer "no":

- the named-capture publication read `.hash` off the Match even when the
  pattern has no named captures;
- `store_through_cell` viewed both the old and the new contents of the `$/`
  cell;
- the closure call's callsite-line marker scan, its junction-autothread scan,
  its bare-block topic pick, its private-attribute guard (which viewed the
  argument before asking whether the body touches any `$!attr`) and its
  captured-cell refresh;
- the binder's internal-named-argument check and its positional/named splits
  (six scans, now one tag-gated `is_pair_arg`).

Now none of them force it, and the only materialization left is the one
`$/` publication after the walk. These scans run on every closure call, so a
block that takes a Match from any source now keeps it lazy too.

On the closure line of `bench-regex-global.raku` (40 occurrences per
operation; `profiling` build, `MUTSU_JIT=off`, callgrind):

- instructions per operation: **1.68M -> 1.37M (-18%)**;
- allocations per occurrence: **52 -> 36 (-31%)**;
- local wall clock for 45 operations: 9.9 -> 7.4 ms.

Refs #19296.
