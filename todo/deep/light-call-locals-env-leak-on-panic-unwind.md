# The positional-light fast call path leaks `self.locals`/`self.env` on a panic unwind

Found while building the regression test for
`todo/tickets/readonly-param-mark-leaks-on-panic-unwind.md` (fixed via
`ReadonlyFrameGuard`, see
`news/2026-08/readonly-param-mark-panic-unwind-raii-guard.md`). This is a
**separate, pre-existing bug**, unrelated to readonly marking, in the same
function that bug lived in.

## Repro (confirmed on a build with the readonly-mark fix applied)

```raku
sub victim($desc) {
    my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic (index-OOB add overflow)
}
my $desc = 'outer-initial';
try { victim(999) };
say "after: $desc";   # !!! prints "after: 999", not "after: outer-initial"
```

Even with no assignment at all after the `try{}`, merely *reading* the
completely unrelated, same-named outer lexical `$desc` after `victim`'s
panicking call returns the panicking callee's OWN argument value (`999`)
instead of the outer variable's real, untouched value (`outer-initial`). This
is silent value corruption, not an exception — arguably worse than the
readonly-mark bug, which at least failed loudly.

## Root cause (traced, not yet fixed)

`call_compiled_function_positional_light`
(`src/vm/vm_call_light.rs`) — the same fast call path the readonly-mark
ticket's fix targeted — explicitly bypasses `push_call_frame`/`run()` for
performance (see the function's own doc comments). It manages a large amount
of caller-side state with plain, sequential save/restore statement pairs
rather than RAII, all of which sit strictly *before* and *after* the body's
`exec_one`/JIT loop (roughly lines 72–410 in the current source):

- `saved_locals` / `self.locals` (lines 72, ~404): the callee's own locals
  array is installed via `take_locals_from_pool`, and the caller's is only
  restored (`std::mem::replace(&mut self.locals, saved_locals)`) AFTER the
  body loop completes.
- `caller_env` / `self.env_mut()` (lines 116–124, ~421–478): a scoped-child
  env is installed for the callee's own writes; the caller's env is only
  restored (merging the overlay back) after the body loop completes.
- `saved_loop_local_vars`, `saved_loop_local_saved_env`,
  `saved_active_loop_param_names`, `saved_block_declared_vars` (lines
  81–93, restored ~406–409): same shape.
- `saved_when_matched` (line 307, restored ~378), `saved_pragmas` (line 276,
  restored ~381), `saved_package` (line 271, restored ~479), `saved_line`
  (line 267, restored ~403), the `push_routine_with_location`/`pop_routine`
  pair (lines 290/373): same shape.

`recover_call_frames_after_panic` (the `catch_unwind` boundary's rollback,
invoked from `run_range_guarded`/`run_inner_guarded`) only knows about state
pushed onto `self.call_frames` plus three other explicitly-tracked side-channel
stacks (`caller_env_stack`, `let_saves`, `test_assertion_line_stack`) — see
its doc comment in `src/vm/vm_env_helpers.rs`. None of the state listed above
is on any of those stacks (this fast path bypasses `push_call_frame`
specifically to avoid that overhead), so a Rust panic raised inside the body
loop (between the "install callee state" prologue and the "restore caller
state" epilogue) leaves ALL of it — not just the readonly mark that the
sibling ticket already fixed — permanently substituted with the panicking
callee's own values. The caller resumes execution (e.g. after a `try{}`)
still running on the callee's locals array and env, silently reading/writing
through leaked callee state instead of its own.

`call_compiled_function_positional_light`'s typed cousin
(`src/vm/vm_call_light_typed.rs`) has the identical shape and is presumably
affected the same way (not independently re-verified for this file, but it
mirrors the untyped path closely enough that it is very likely a mirror bug).

## Why this is filed separately rather than folded into the readonly-mark fix

- Different backing state entirely (`self.locals`/`self.env` and several
  other caller-side save/restore locals, not `readonly_vars`/
  `readonly_undo`/`readonly_frames`).
- Much larger blast radius: fixing it properly likely means either (a)
  converting each of these ~8 save/restore pairs into its own RAII guard
  (following the `vm_call_state_guard.rs` v3 recipe — several of these are
  non-`Copy`/collection-shaped, similar to `ThreadParamMaskGuard`'s two
  `HashSet` fields, so this is not a small mechanical change), or (b)
  restructuring this fast path to register with `call_frames` after all
  (which the function's own doc comments say was deliberately avoided for
  performance) or with some other rollback-aware mechanism, or (c) wrapping
  just the body-execution loop in a local `catch_unwind` inside this
  function and unconditionally restoring everything (readonly included) in
  both the `Ok` and `Err` arms before resuming the unwind. Each option is a
  genuine design decision, not obviously "the same shape" as the
  single-mechanism `ThreadParamMaskGuard`/`ReadonlyFrameGuard` fixes.
- Landing it inside the readonly-mark PR would have conflated two unrelated
  root causes and made that PR's diff much harder to review.

## Suggested next step

1. Confirm whether `vm_call_light_typed.rs` reproduces the same leak (very
   likely, given the structural mirror) with a typed-signature variant of the
   repro above.
2. Decide the fix shape (RAII guards for each field vs. a local
   `catch_unwind` wrapping the body loop vs. routing through `call_frames`
   after all) — this is a design call, not a mechanical one, given the
   number of independent pieces of state involved and the function's
   explicit performance rationale for avoiding `push_call_frame`.
3. Add a regression test based on the repro above (a `say`, not an
   assignment — reading the leaked value directly demonstrates the
   corruption without depending on the (now separately fixed) readonly-mark
   symptom masking it).
