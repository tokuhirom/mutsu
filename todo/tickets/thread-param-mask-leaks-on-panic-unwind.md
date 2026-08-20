# `mask_thread_redeclared_params` leaks its mask on a panic-unwind through the call it guards

Found while auditing call-dispatch functions for manually-saved-and-restored
state that a Rust panic unwind skips (see
`news/2026-08/panic-unwind-call-dispatch-state-raii-guards.md`, which fixed
`current_package`, pragma state, `state_scope_id`, `when_matched`, and three
Vec-shaped side-channel stacks the same way).

`call_compiled_function_named_inner` (`src/vm/vm_call_named_inner.rs`) calls

```rust
let masked_params = self.mask_thread_redeclared_params(cf.param_defs.iter());
...
self.unmask_thread_redeclared_params(&masked_params);
```

`mask_thread_redeclared_params` (`src/runtime/runtime_shared_vars.rs`) inserts
each parameter's bare name into two process-wide `HashSet<String>` fields --
`thread_redeclared_vars` and `thread_param_shadow_vars` -- recording in the
returned `ThreadParamMask` exactly which names THIS call's mask added (as
opposed to a name that was already present from an ancestor frame's own
mask), so `unmask_thread_redeclared_params` can remove precisely those and no
others. This machinery only activates when `self.shared_vars_active` (a
cross-thread `start`/`Promise`/shared-var program is running).

Like `current_package`/`state_scope_id`, `unmask_thread_redeclared_params` is
called as a plain statement near the end of a large `&mut self` function. A
Rust panic raised inside the call body (between the mask and unmask) unwinds
straight past it -- only `Drop` runs on unwind -- leaving both `HashSet`s
holding this call's mask entries after the panic is caught at the enclosing
`try`/`EVAL` boundary. Concretely, a bare name that should be looked up in the
shared-var store again (because this call's shadowing parameter is gone) may
continue resolving to a stale masked/shadowed state until some LATER call
happens to mask+unmask the same name again, which naturally papers over the
leak in many programs -- likely why this has not surfaced as an obvious test
failure yet.

## Why this needs a different fix shape than the other guards

The other guards fixed in the linked news entry restore a **single scalar
field** (or truncate a **Vec back to a recorded depth**) -- both trivial to
wrap in `Drop`. This one inserts into a **shared, process-wide `HashSet`**
where "this call's contribution" is a specific, already-computed set of
names (`ThreadParamMask { redeclared: Vec<String>, shadowed: Vec<String> }`),
not a depth. An RAII guard IS still the right mechanism (construct it right
after `mask_thread_redeclared_params` returns, holding the `ThreadParamMask`
and a way to reach `thread_redeclared_vars`/`thread_param_shadow_vars` on
drop), but those two fields are plain (non-`Arc`) fields on `Interpreter` in
a large dispatch function -- the same "raw `*mut Interpreter` captured at
construction" pattern used for `StateScopeGuard`/`WhenMatchedGuard`
(`src/vm/vm_call_state_guard.rs`) should work here too; reuse that pattern
rather than inventing a new one.

## Repro sketch (unverified -- needs a session to actually build and confirm)

Something in the shape of: a `start { ... }`/`await` program where a named
sub's parameter shadows a shared bare-name variable, the sub's body panics
(e.g. an intentionally huge array index), the panic is caught by an enclosing
`try`, and a subsequent read of the bare name (from a different thread/frame)
observes the stale masked/shadowed state instead of falling through to the
shared-var store. This ticket does not include a confirmed-reproducing
example -- building one requires exercising the cross-thread shared-var path
deliberately, which the fixing session should do before writing the fix.
