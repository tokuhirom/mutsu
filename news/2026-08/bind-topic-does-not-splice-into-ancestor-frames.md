# A `:=` bind from the topic no longer corrupts every ancestor call frame

`roast/S03-operators/range.t` regressed under `MUTSU_REAL_TEST=1` (the vendored,
real `Test.rakumod` behind the `vendor-real-test-module.md` campaign) with a
`for` loop's own topic corrupted mid-file: after the first `throws-like` call
inside `for @opvariants { ... }`, `$_` permanently became whatever the
exception's message text was, so every later statement's `"...$_..."`
interpolation built garbage source and failed to parse.

This was previously investigated (`todo/deep/module-catch-default-topic-leaks-to-callers-for-loop.md`)
and misdiagnosed as a `CATCH { default { } }` topic-scoping bug needing "a real
topic stack" design. That diagnosis was wrong. The actual root cause, found via
`rust-gdb`/one-shot instrumented builds (never printf-in-a-loop debugging) that
traced every `Env` mutation and `call_frames` push/pop across the failing call
chain: `Test.rakumod`'s `throws-like` does `my $ex := $_;` inside its
`CATCH { default { ... } }`, and mutsu's `:=` bind machinery
(`vm_var_assign_set_local.rs`'s `exec_set_local_op_inner`, plus four sibling
sites in `vm_var_assign_coerce.rs`/`vm_exec_dispatch.rs`/`vm_env_helpers.rs`)
has a "promote the bind source to a shared `ContainerRef` cell and splice that
cell into every ancestor call frame that owns the lexical, so the sharing
survives a `return`" mechanism — intended for a genuine outer lexical
(`sub f($x is rw) { my $c := $x; ... }`).

The gate for "does an ancestor frame own this lexical" was
`frame.saved_env.contains_key(&resolved_source)` — which, for a *scoped* env,
walks the whole parent chain. The topic `$_` is not a genuine lexical any one
frame owns; it is a fresh per-call binding every routine writes into its own
env on entry (`vm_call_named_inner.rs`'s `is_routine` reset). That makes it
chain-visible from essentially every frame on the stack, so the gate reported
"yes, some outer frame owns `_`" for nearly all of them — and the propagation
loop then spliced the exception object (wrapped in a shared cell) directly into
each ancestor's own saved env, including frames many levels up the call stack
that had nothing to do with the bind.

Two fixes, both needed:

1. `Env::contains_key_own_tier` — a same-frame-only membership check (no parent
   chain, no global-base fallthrough) — replaces the chain-walking
   `contains_key` at all five "propagate a promoted cell to ancestor frames"
   sites, so the loop no longer treats "visible via the chain" as "owned by
   this frame."
2. `vm_var_assign_set_local.rs`'s `source_in_outer_frame` gate now excludes the
   per-call pseudo-variables (`_`, `@_`, `%_`, `!`) outright: a `:=` bind only
   needs to capture the source's *current* referent in the target name, and
   `_`/`@_`/`%_`/`!` are never a genuine outer lexical worth promoting to a
   shared cell across a return boundary in the first place.

Pin: `t/bind-topic-does-not-corrupt-ancestor-frames.t` — a plain (non-Test.rakumod)
`sub inner() { my $ex := $_; $ex.defined; }` called from inside a `for` loop,
confirmed to reproduce the corruption before the fix (`',b,c'` instead of
`'a,b,c'`) and pass after. `roast/S03-operators/range.t` now passes fully under
both the native and the real `Test` provider; the original ticket's own minimal
repro (a `for` loop calling `throws-like` directly) is fixed under both
providers too. Full local `t/` suite (3220 files, 29897 tests) and
`cargo clippy -- -D warnings` both clean.
