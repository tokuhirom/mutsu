# A routine parameter's readonly mark no longer leaks past a panic-unwound call

A routine parameter gets Raku's default readonly mark on entry
(`Interpreter::readonly_vars`, via `mark_readonly`/`mark_readonly_sym`) and is
supposed to lose it again once the call returns, via `exit_readonly_frame`.
On the positional-light fast call path
(`call_compiled_function_positional_light` in `src/vm/vm_call_light.rs`, and
its typed cousin in `src/vm/vm_call_light_typed.rs`) that restore was a
plain, sequential `self.exit_readonly_frame(saved_readonly)` statement near
the function's end. This fast path deliberately bypasses `push_call_frame`
for performance, so the readonly scope it opened was invisible to
`recover_call_frames_after_panic` — the rollback the `catch_unwind` boundary
(`run_range_guarded`/`run_inner_guarded`) runs on a caught panic. A Rust
panic raised inside the callee's body (an index-OOB add overflow, the same
construct `t/vm-panic-boundary.t` uses) unwound straight past that statement
— only `Drop` runs on unwind — permanently leaving the parameter's bare name
marked readonly, corrupting any later, completely unrelated same-named
lexical:

```raku
sub victim($desc) {
    my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic
}
my $desc = 'outer-initial';
try { victim(999) };
$desc = 'outer-updated';   # used to throw: "Cannot assign to a readonly variable (desc) or a value"
```

This is the sibling of the `thread_redeclared_vars`/`thread_param_shadow_vars`
leak fixed via `ThreadParamMaskGuard` (see
`news/2026-08/thread-param-mask-panic-unwind-raii-guard.md`) — a completely
separate backing mechanism (a readonly-name registry, not the cross-thread
shared-var mask), reproducing with zero threading involved, that turned out
to suffer the exact same "manually restored, skipped by a Rust panic unwind"
bug shape.

Fixed by adding `ReadonlyFrameGuard` (`src/vm/vm_call_state_guard.rs`),
following the same v3 RAII recipe as `ThreadParamMaskGuard`,
`StateScopeGuard`, and the other guards in that file: `readonly_vars`
(`FxHashSet<Symbol>`) and `readonly_undo` (`Vec<ReadonlyUndo>`) are now boxed
as `Box<RefCell<_>>`, and `readonly_frames` as `Box<Cell<u32>>` — each its own
heap allocation, separate from `Interpreter`'s own, so a raw pointer into it
stays valid across intervening `&mut self` calls (per the module doc's "v3"
section, and unlike the two unsound raw-pointer-into-`Interpreter` designs
documented there). The guard's `Drop` replays the readonly-set rollback
through those pointers via a new shared helper,
`crate::runtime::replay_readonly_undo`, which both the guard and
`Interpreter::exit_readonly_frame` itself call — so the rollback logic lives
in exactly one place. `take_readonly_state`/`restore_readonly_state` (used by
the gather/lazy-list force path) were updated to swap the boxed cells'
*contents* in place rather than replacing the `Box`es themselves, so a live
guard further up the call stack is never left holding a pointer into a freed
allocation.

`readonly_vars`/`is_readonly_sym` sits on the hottest possible path — checked
on every local variable assignment (`vm_var_assign_set_local.rs`) — so
boxing it into a `RefCell` was a deliberate, measured choice: unlike the
whole-set `Arc`-clone-per-call snapshot design this journal mechanism
replaced (a previously measured ~15% of the hottest call path's self time,
per `enter_readonly_frame`'s doc comment), a `RefCell` borrow-flag check adds
only a few cheap instructions per access, not a clone or a lock.

While building the regression test for this fix, a **separate, pre-existing**
bug surfaced in the same fast-path function: even with the readonly-mark leak
fixed, `self.locals`/`self.env` are *also* left holding the panicking
callee's own state after the same kind of panic, so a later, unrelated read
of the same-named outer lexical silently returns the callee's leaked argument
value instead of throwing. This is unrelated to readonly marking and was
filed separately as
`todo/deep/light-call-locals-env-leak-on-panic-unwind.md`.

Pinned by `t/readonly-param-mark-panic-unwind.t`.
