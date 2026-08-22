# Fixed the positional-light fast call path leaking `self.locals`/`self.env` on a panic unwind

Found while building the regression test for
`todo/tickets/readonly-param-mark-leaks-on-panic-unwind.md` (fixed via
`ReadonlyFrameGuard`, see
`news/2026-08/readonly-param-mark-panic-unwind-raii-guard.md`). This was a
**separate, pre-existing bug**, unrelated to readonly marking, in the same
fast-call functions that bug lived in — and, unlike the readonly-mark bug,
it failed *silently* rather than throwing.

## The bug

```raku
sub victim($desc) {
    my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic (index-OOB add overflow)
}
my $desc = 'outer-initial';
try { victim(999) };
say "after: $desc";   # printed "after: 999", not "after: outer-initial"
```

Merely *reading* a completely unrelated, same-named outer lexical after a
panicking call returned the panicking callee's OWN argument value instead of
the outer variable's real, untouched value — silent value corruption, not an
exception.

`call_compiled_function_positional_light` (`src/vm/vm_call_light.rs`) and its
named-arg-capable cousin (`call_compiled_function_light`/
`call_compiled_function_light_spec`, `src/vm/vm_call_light_typed.rs`) both
bypass `push_call_frame`/`run()` for performance and manage a large amount of
caller-side state (`self.locals`, `self.env`, the loop/block-scope save sets,
`when_matched`, pragmas, the current package/source line, and the
routine-stack push/pop) via plain, sequential save-before/restore-after
statements. None of that state was registered on any rollback list
`recover_call_frames_after_panic` (the top-level `catch_unwind` boundary's
rollback) knows about, so a Rust panic raised inside the callee body unwound
straight past every restore statement, leaving the caller running on the
panicking callee's own locals/env for the rest of the program.

Confirmed the named-arg-capable cousin (`call_compiled_function_light_spec`)
was affected too, via a `:$desc`-parameter variant of the repro (verified by
checking the panic backtrace names the expected function).

## The fix

Unlike the single-field `ReadonlyFrameGuard`/`ThreadParamMaskGuard` bugs this
mirrors, there is no single `Box<Cell<_>>` this state can be moved behind for
an RAII guard following the `vm_call_state_guard.rs` v3 recipe: `self.locals`
and the other fields involved are plain, directly-embedded `Interpreter`
fields mutated by thousands of call sites throughout the VM, so boxing them
behind interior mutability to fit that pattern would be an unrelated, much
larger refactor.

Instead, each function wraps just its body-execution loop in a local
`std::panic::catch_unwind(AssertUnwindSafe(...))`. On the `Ok` path nothing
changes. On `Err(panic_payload)`, the function restores every piece of
caller-side state exactly as its normal completion path already did, then
calls `std::panic::resume_unwind` to continue the panic outward (so an
enclosing `catch_unwind` boundary, e.g. a `try{}`, still sees a real panic and
converts it to a catchable `X::AdHoc`, per `t/vm-panic-boundary.t`). The
trickiest piece — the env-overlay merge back into the caller — was factored
into a small private helper (`finish_positional_light_env` /
`finish_light_env`) shared between the normal path and the panic-recovery
arm, to avoid literally tripling that ~35-line block within one function.

This costs nothing on the non-panicking hot path: `catch_unwind` has no
runtime overhead unless a panic actually occurs, confirmed with a release-build
`fib(32)` A/B (fixed: ~1.32s across 3 runs; pre-fix baseline: ~1.33-1.35s —
within noise, no regression).

## Tests

`t/light-call-state-leak-on-panic-unwind.t` exercises both call paths
(positional-only and named-arg) with the exact repro, asserting the outer
lexical is untouched after the panicking call. `t/readonly-param-mark-panic-unwind.t`
(the sibling ticket's test) was also strengthened to assert the outer
lexical's value directly now that this bug is fixed, not just that the write
no longer throws.
