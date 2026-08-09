# `.map`/`.grep` inline blocks no longer abort on a matched `when`/`default`

`(1,2).map({ when Int { "int" } })` died with an empty `Runtime error:`
instead of returning `(int, int)`; `.grep` with the same shape was
equally affected. A direct closure call (`{ when Int {...} }(5)`) already
worked — only the `.map`/`.grep` inline fast path was broken.

## Root cause

A matched `when`/`default` raises a "succeed" control signal carrying the
matched branch's value (`exec_when_op`/`exec_default_op` in
`src/vm/vm_given_when_ops.rs`). This signal is normally absorbed at the
enclosing block boundary — a closure call
(`src/vm/vm_closure_dispatch.rs`) or a statement-level `SucceedBarrier`
(`src/vm/vm_control_ops.rs`). But `.map`/`.grep` run their block's
compiled code inline via `vm.run_reuse(...)` for performance, bypassing
both of those absorbing frames. The inline loops' error handling only
matched `is_next()`/`is_last()`; a succeed signal fell into the generic
`Err(e)` arm and propagated out of the whole map/grep as an
empty-message `RuntimeError`.

## Fix

Added a `is_succeed()` arm to every inline map/grep runner that lacked
one, mirroring the closure-call boundary: treat the signal's
`return_value` as the item's produced value (for map) or predicate result
(for grep), and reset the when-matched flag the same way
`exec_succeed_barrier_op` does so an enclosing `given` doesn't see a stale
match. Fixed sites: `eval_map_over_items` and the `.first`-style
`'body_redo` scan loop in `src/runtime/resolution_map_grep.rs`, plus
`eval_map_over_items_rw` and `eval_grep_over_items_with_mutated` (the rw
map/grep paths) in `src/runtime/resolution_map_grep_rw.rs`. The
`find_first_match_generic`/`try_first_match_batched` paths (`.first`)
were already correct — they call through the full closure-call boundary
rather than the inline fast path.

## Verification

- `(1,2).map({ when Int { "int" } })`, `.grep` with the same shape, and
  `map`+`default` all now match raku.
- The `--> Supply` method shape used by Cro's
  `Cro::HTTP::BodySerializer::MultiPartFormData.serialize` (the original
  real-world trigger) no longer aborts.
- `t/http-request-serializer.rakutest` (vendored Cro::HTTP suite): test
  16 no longer aborts the file — it now hits the next, separately
  tracked blocker ("Prohibited regex interpolation").
- New pin: `t/map-when-succeed.t` (passes under both `mutsu` and `raku`).
- The whitelisted `given`/`when`/map/grep roast files (14 files, 341
  subtests) pass with no regressions.

## Follow-up

Verifying this fix against inputs with genuinely non-matching items
surfaced a separate, pre-existing bug: a block whose only statement is a
`when`/`default` chain that does NOT match any branch produces the wrong
value (it falls back to the raw topic instead of raku's actual
non-match value). This was previously unobservable because any match in
the same map/grep call aborted the whole call before the wrong fallback
could surface. Tracked separately in
`todo/tickets/when-only-block-nonmatch-value-wrong.md`.
