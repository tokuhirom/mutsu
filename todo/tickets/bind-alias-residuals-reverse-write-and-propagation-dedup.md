# `:=`-bind residuals: reverse-direction alias write, and the duplicated ancestor-frame propagation loops

Follow-ups split out of the resolved
`news/2026-08/attr-bind-source-write-tracked-through-nested-call-chain.md`
(formerly `todo/deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md`).
The source-write-tracking direction is fixed and pinned by
`t/bind-source-tracks-through-call-chain.t`; two adjacent problems remain.

## 1. Reverse-direction write: `$alias = 5` does not reach `$var` when the bind ran inside a sub

```raku
my $var = 100;
my $alias;
sub bindit { $alias := $var }
bindit();
$alias = 5;
say $var;   # raku: 5   mutsu: 100
```

Pre-existing (not a regression from the fix). The forward direction
(`$var = ...` observed through `$alias`) works because the bind now reuses
`$var`'s own authoritative cell; but mainline's write to `$alias` still lands
on `$alias`'s *pre-bind* boxed cell (its ADR-0024 mainline capture /
closure-capture cell), which after the bind merely CONTAINS the shared cell
rather than BEING it. A write through `unit_scope_lexical_write` /
`set_env_with_main_alias` does `cell.lock().clone_from(val)` on the outer
cell, replacing the nested `ContainerRef` instead of writing through it. Fix
sketch: either replace the alias's store entries (unit store, local slot, env)
with the shared cell itself at bind time (no nesting), or make the by-name
cell write-through deref nested `ContainerRef` contents before storing
(mirroring `Value::store_through_cell`'s HashEntryRef materialization). The
first is cleaner but must update every store that currently holds the old
cell.

## 2. The ancestor-frame propagation mechanism is duplicated, and its `saved_locals` patch is still indexing with the wrong frame's layout

Both `:=`-bind handlers — the `SetLocal` one
(`src/vm/vm_var_assign_set_local.rs`, two branches) and the `SetGlobal` one
(`src/vm/vm_exec_dispatch.rs`) — carry a near-identical inline block: resolve
the sigilless alias chain, mint/reuse the shared cell, splice it into every
ancestor `frame.saved_env` owning the name (`contains_key_own_tier`), and try
to patch `frame.saved_locals[i]` by searching **the currently-executing
frame's** `code.locals` — which is the wrong frame's slot layout, so that
inner loop is a no-op for exactly the cross-frame free-variable case it
exists for (`VmCallFrame` carries no per-frame locals-name table; see
`src/vm.rs`). The stale `saved_locals` slots are mostly papered over by the
post-return lazy sync (`exec_set_local_op_inner`'s "adopt ContainerRef from
env" block), which is why the fixed tests pass, but the mechanism should be
unified into one helper and the `saved_locals` patch either made correct (per-
frame locals-name table on `VmCallFrame`) or deleted as dead code with the
lazy sync documented as the real carrier.
