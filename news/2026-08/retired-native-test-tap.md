# The native `Test::Tap` provider is retired

`use Test::Tap` was a native no-op in `src/runtime/runtime_module.rs`:
roast's real `packages/Test-Helpers/lib/Test/Tap.rakumod` was never loaded and
`tap-ok` was answered by `src/runtime/test_functions/tap_ok.rs`. That is a
rung-3 native provider in the sense of `docs/BATTERIES.md`, and the same kind of
thing `Pod::To::Text` and the `monitor` declarator were retired for. It was also
a hard blocker for `todo/tickets/vendor-real-test-module.md` step 3: under
`MUTSU_REAL_TEST=1` the 44 whitelisted files that `use Test::Tap` kept two
counters — the module's `plan`/`ok` and the native `tap-ok` — so each reported
`# You planned N tests, but ran 0` at END even though every `ok` line printed.

Deleting the arm is one line. Making it work took six unrelated interpreter
fixes, because the real `tap-ok` asserts with the real `is-deeply` where mutsu's
native one was lenient, and it exercises `Supply` combinators the native one
never touched. Every one had a `raku`-passing repro with no `Test` involved:

| what the real `tap-ok` exposed | fix |
| --- | --- |
| `Supply.live` was an attribute accessor, not a method, so a combinator result died with `No such method 'live'` | `news/2026-08/live-supply-combinators.md` |
| `Supply.merge` over live sources snapshotted an empty `values` and finished immediately | same |
| `Supply.reduce` over a live source did nothing at all (its `reduce_source` attribute was written and never read) | same |
| `classify`/`categorize` group supplies were not preserving, so the usual late tap saw only `done` | same |
| `Supply.rotor` emitted `List`s where rakudo emits `Array`s | same |
| `Supply.interval(:$scheduler)` never called `.cue`, so a user-written scheduler drove nothing | `news/2026-08/scheduler-driven-supply-interval.md` |

A seventh surfaced once the module's own signatures came into play: a `&`-sigil
*named* parameter never bound, so `tap-ok`'s `:&emit`/`:&done`/`:&after-tap`
were always undefined and every `after-tap() if &after-tap` guard silently
skipped (`news/2026-08/named-callable-parameter-binds.md`).

All 44 files now pass with the real module, and the full `make roast` (1435
files) is green with the intercept gone. `src/runtime/test_functions/tap_ok.rs`
is deleted along with its `TEST_MODULE_EXPORTS` / `is_test_function_name` /
`compile_consts` entries, and with the counter-mode `FakeScheduler` cue that
existed only to serve it. Three `t/` files that had been relying on the native
provider — two of them with a `use lib` path that never resolved, which the
native provider hid — now load the real module too.

The fudge-preprocessing entries for `tap-ok` in `run_roast_preprocess.rs` stay:
those count it as a test assertion for `#?rakudo skip`, which has nothing to do
with who provides it.

## What the earlier survey got wrong

Five of the six regressions were attributed to a single cause — "a tap
callback's `@res.push($_)` collects nothing when the emit runs on a
timer/scheduler thread", filed as an `@`-aggregate lane bug in the cross-thread
shared store. Measured file by file they had five different causes, none of them
that one, and the shared store was not involved in any of them. The ticket's own
repro turned out to be a *seventh* unrelated bug — `.head` on a channel-backed
Supply, where the tap callback never runs at all, so no push is ever attempted
(`todo/tickets/head-on-a-channel-backed-supply-drops-every-value.md`).
