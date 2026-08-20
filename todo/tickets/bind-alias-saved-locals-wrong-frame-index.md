# `:=`-bind ancestor-frame propagation: `saved_locals` patch is still indexing with the wrong frame's layout

Follow-up split out of the resolved
`news/2026-08/attr-bind-source-write-tracked-through-nested-call-chain.md`
(formerly `todo/deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md`).
The source-write-tracking direction is fixed and pinned by
`t/bind-source-tracks-through-call-chain.t`, and the reverse-direction write
(`$alias = 5` reaching a source bound inside a sub) is now fixed too — see
`news/2026-08/bind-alias-reverse-write-through-nested-cell.md`, pinned by
`t/bind-alias-reverse-write.t`. What that session also did as a low-risk
cleanup: the ancestor-frame propagation loop that used to be duplicated
near-identically across the `SetLocal` (`vm_var_assign_set_local.rs`, two
branches) and `SetGlobal` (`vm_exec_dispatch.rs`) `:=`-bind handlers is now a
single shared helper, `Interpreter::propagate_bind_to_ancestor_frames`
(`src/vm/vm_var_assign_ops.rs`). This ticket is what's left: the helper's
`saved_locals[i]` patch loop is still indexing with the wrong frame's layout
for the general cross-function case, and remains un-fixed (not deleted,
because it is not unconditionally dead — see below).

## The residual bug

`propagate_bind_to_ancestor_frames(name, code, container)` walks
`self.call_frames` in reverse and, for each ancestor `frame` whose
`saved_env` owns `name`, splices `container` into both `frame.saved_env` (by
name — correct) and `frame.saved_locals[i]` where `i` is found by searching
**`code`, the CURRENTLY EXECUTING frame's own locals array** for `name`.

For the common case this mechanism exists for — a `:=` bind performed inside
a callee, naming a free/outer lexical owned by some ancestor frame belonging
to a *different* compiled function — `code.locals`'s slot layout has nothing
to do with `frame`'s own layout, so the index `i` (if found at all) patches
the wrong slot in `frame.saved_locals`, or an unrelated local at that index
in a frame that happens to have enough slots. `VmCallFrame` carries no
per-frame locals-name table to look up the *correct* index by (see
`src/vm.rs`), so there's no cheap fix at this call site.

**It is not unconditionally a no-op**, which is why it was kept rather than
deleted outright: when `frame` is a *recursive* invocation of the exact same
compiled function as `code` (e.g. a bind performed inside a recursive
call that names a lexical from an outer, still-live invocation of the same
sub), the two locals layouts coincide by construction and the patch happens
to land on the right slot. That narrow case was not proven either way to be
reachable/meaningful in practice, so pinning it down (or confirming it never
actually matters, letting the loop be deleted) is future work.

The bug is mostly invisible today because of a **second, independent**
mechanism that already carries the binding correctly for the general case:
`frame.saved_env`'s by-name splice (right above the broken loop) means that
when an ancestor frame's env is restored on return, its `saved_env` already
holds the shared `ContainerRef`. The "lazy sync" block in
`exec_set_local_op_inner` (`vm_var_assign_set_local.rs`, search for "Lazy
sync: if the local is not a ContainerRef but env has one") then adopts that
`ContainerRef` out of `env` into `locals` on the next read/write of the name
in that frame. This is why `t/bind-source-tracks-through-call-chain.t` and
`t/bind-alias-reverse-write.t` both pass despite the `saved_locals` patch
being unreliable.

## What to do

Either:

1. **Make it correct.** Add a per-frame locals-name table to `VmCallFrame`
   (its own `code.locals`-equivalent, captured at call time) so the helper
   can look up the *right* index for `frame` instead of borrowing `code`'s.
   This is the "clean" fix but touches `VmCallFrame`'s shape and every call
   site that constructs one — audit the blast radius before starting.
2. **Delete it as confirmed-dead code**, after first proving the
   same-function-recursion case never actually needs it (write a regression
   test for a recursive sub performing a `:=` bind of an outer, still-live
   invocation's lexical, run it against both the current code and a version
   with the loop deleted, and confirm identical behavior). If the loop turns
   out to matter for that case even once, option 1 is the only sound path.

Either way, the `saved_env` splice + `exec_set_local_op_inner`'s lazy sync
stays as the real, working general-case mechanism; this ticket is only about
the `saved_locals` loop's correctness/removal.
