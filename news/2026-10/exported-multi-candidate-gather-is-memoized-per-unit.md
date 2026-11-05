# An exported multi family's candidate list is memoized per asking unit, and a dispatcher call no longer walks the registry

A call through an exported `multi` dispatcher value (`Array::Sorted::Util`'s
`my proto sub inserts(|) is export {*}`, #15786) did two O(registry) things per call:

- `resolve_all_multi_candidates_cached_sym` bypassed its memo for any family with import
  records (#11004: the list depends on the asking unit), so the whole gather ran every call.
  The units that decide it, `(current unit, executing unit, import-table generation)`, now join
  the memo key, as they do the dispatch plan's, instead of bypassing it.
- `captured_candidates_are_live` (run on every call of a dispatcher value) used the `&self`
  full walk of the functions map. It now reads the base-name key index when that already holds the
  name (the steady state), which `indexed_gather_equivalence_tests` pins as the same answer.

Measured with the profiling build against the main build taken just before (Ir per
`inserts @k, $_ % 5, @v, $_, :force` call, slope between 40 and 120 calls, deterministic):
4,111,569 -> 3,779,432 (-8.1%); the issue's loop (1000 calls) 0.93 s -> 0.80 s wall. What is left
is flat: 6 frames and 30 `&cmp` value calls per insert (about 15k Ir each) over a 3.8M-Ir call,
no single function above ~5%.
