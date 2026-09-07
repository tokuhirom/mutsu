# A multi's dispatch frame reuses its candidate list, and a chain flatten clones the scope once

Fifth perf slice for `todo/deep/vendor-real-test-module.md`. Two fixes on
the largest rows left in the per-assertion profile under the vendored
`Test.rakumod`:

- **Every call of a `multi` re-gathered its full candidate list.**
  `push_multi_dispatch_frame` -- which every `multi` call runs so that
  `callsame`/`nextsame` have somewhere to defer to -- called
  `resolve_all_multi_candidates_indexed` per call: build the package list,
  format one `Pkg::name/` prefix per package, resolve the proto owner,
  filter the base-name key index, dedupe by body fingerprint, sort by
  specificity. `ok` and `is` are multis, so that ran once per assertion,
  rebuilding a list that had not changed since the previous call.
  `resolve_all_multi_candidates_cached` memoizes the shared list per
  `(name, current package, frame lexical package)` -- the inputs the gather
  reads besides the registry -- and drops the memo when `fn_resolve_gen`
  (every function registration or removal) or the registry's `proto_gen`
  moves. The existing gather-equivalence unit test pins that a registration
  after a hit refreshes the answer.
- **`Env::flattened` on a two-tier chain cloned the whole scope twice.**
  It recursed through `parent.flattened()` and then cloned *that* result to
  layer its own overlay on top, so a method call two routine frames deep
  (`ok` -> `proclaim` -> `$output.say`) materialized the mainline scope
  once per tier. It now walks to the flat root, clones that map once, and
  layers every tier's tombstones and overlay on it root-ward first -- the
  single pass `filtered_flat` already used.

## Measured

Callgrind, 300 `ok 1, "x"` under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted:

| | per assertion |
| --- | --- |
| before (after the call-return slice) | 352,748 Ir |
| after | 333,577 Ir |

-5.4% on this slice, -32.2% since the session started at 492,188. The
`push_multi_dispatch_frame` row (9,535 Ir per assertion) is gone from the
profile; `Env::flattened` went 24,619 -> 15,944 and the scope-map clone
inside it 16,868 -> 12,143. `roast/S03-buf/write-int.t` under the real
module: 9.9 s (median of three) on the same box, 13.8 s at the start of
the session.
