# ADR-0019 E5 step 4: measurement counters for call_method_all_with_fallback

Instrumented `call_method_all_with_fallback` (`src/vm/vm_call_helpers.rs`)
with the same `MUTSU_VM_STATS`-gated dispatch-entry counters used by the
prior E5 measurement slices, `entry = "callmethodallfallback"`. This is the
fourth and last of the E5 measurement entries named in the E5-E7 design
doc's decision 4, completing the measurement phase for Phase E's "ordinary
VM method calls" box.

Unlike the opcode handlers instrumented in earlier steps, this is a single
shared helper function with a trivial two-outcome body (`native`/`user`),
called from 6 sites across 5 files: `CallMethod`'s own `.+`/`.*` modifier
arms, `CallMethodMut` and `CallMethodDynamicMut` (E6 territory — not yet
measured independently), and three sites unrelated to the `.+`/`.*`
modifiers at all (`.cache`/`.Map` coercions, a cached scalar-accessor
probe). Pure insertion, zero behavior change.

A full `t/` sweep found 7 files exercising this helper (`user=22`,
`native=3`), all confirmed by inspection to be `.+`/`.*` MRO-walk tests on
variable receivers — i.e. routed through the `Mut` opcodes rather than
`CallMethod` directly, the same "bareword/variable receiver picks the Mut
opcode" pattern earlier steps documented. The sample is small enough that
this helper's real traffic profile will only be clear once the E6
measurement slice for `CallMethodMut` runs.

`make test` (full `t/`, 3018 files, 28265 subtests) passes unchanged (one
known-flaky concurrency test, `t/supply-done-in-tap-callback-is-not-a-failure.t`,
failed once and passed 5/5 on immediate re-runs — unrelated to this change).

With all four E5 measurement sub-slices done, E5b (the `CallMethod`
probe-section cutover to the E4 resolver decision) is next. Full detail:
`todo/deep/adr0019-e5-e7-entry-routing.md` (§"Measurement slice results —
call_method_all_with_fallback (E5 step 4)") and
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` (E5
bullet).
