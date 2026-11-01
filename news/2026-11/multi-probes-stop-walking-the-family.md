# Multi-candidate probes stop walking a name's whole family on every call

Profiling a real Red 0.2.7 model (`Bla.^create(:bla($_)) for ^N`, callgrind slope between two
iteration counts, profiling build, warm) put one `^create` at 811M Ir. Three independent causes
kept the `has_multi_function` / `has_multi_candidates` probes walking every key of a name's
family (~1,400 keys for Red's operators, ~44 probes per create) instead of hitting their memos:

- `dispatch_interpreter_terminal` discarded every function-resolution memo after any interpreter
  carrier that merely took a registry write guard. A re-dispatch (`samewith`, `nextwith`,
  `callwith`, `callsame`, `nextsame`) only runs the next candidate, so it no longer does; one
  `.new` through Red's `callwith`/`samewith` chain used to wipe the memos once per create.
- A name some import scoped (every Red operator) skipped the memos entirely, because the full
  answer depends on the running compunit. The registry half of the answer does not, so
  `has_multi_function` / `has_multi_candidates` now memoise that half under the existing
  generation and family-generation keys, and only the visibility half runs per call.
- `user_function_matches_call` asked the uncached `has_multi_function` right after asking the
  memoised one; and the `&self` probe `has_multi_candidates_unindexed` (used by
  `resolve_function`, ~250 per create) now reads the memo through a new read-only
  `FamilyGenCache::peek`.

Result on the same loop: 811M -> 720M Ir per create (-11%); native, 50 creates 12.98 s -> ~11 s
and `Bla.^all.sort(*.id).Array` of 50 rows 1.82 s -> ~1.5 s (same box, same session; wall clock
drifts, the Ir slope is the evidence). Red's `t/38-equal.rakutest` is still far from its goal
(#15498); the profile that follows is flat across type matching and multi dispatch.
