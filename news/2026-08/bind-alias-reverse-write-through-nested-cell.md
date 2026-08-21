# A `:=`-bound alias is now a genuinely writable alias, even when the bind ran inside a sub

Closes the primary problem in
`todo/tickets/bind-alias-residuals-reverse-write-and-propagation-dedup.md`
(a follow-up split out of
`news/2026-08/attr-bind-source-write-tracked-through-nested-call-chain.md`).
The bug:

```raku
my $var = 100;
my $alias;
sub bindit { $alias := $var }
bindit();
$alias = 5;
say $var;   # raku: 5   mutsu (before this fix): 100
```

The forward direction (a write to `$var` observed through `$alias`) already
worked, because the bind reused `$var`'s own authoritative cell. But a write
*through* `$alias` afterward still landed on `$alias`'s pre-bind storage cell
(its ADR-0024 mainline/closure capture cell), silently discarding the shared
cell rather than writing through it.

Root cause: after the bind, `$alias`'s own outer storage slot (its ADR-0024
capture cell) did not become the shared cell — it merely came to *contain* a
`Value::ContainerRef` pointing at it, a nested-cell shape. A subsequent plain
assignment to `$alias` funneled through `Value::store_through_cell`, which
unconditionally did `inner.clone_from(val)` on the outer cell's contents —
overwriting the nested `ContainerRef` wholesale instead of writing through
it, severing the alias link.

The fix mirrors a pattern the same function already used for a different
container kind: `store_through_cell` had an existing branch that materializes
a `HashEntryRef` deferred token before storing, so a hash alias isn't dropped
by a naive overwrite. A new sibling branch does the analogous thing for a
nested `ContainerRef`: when the cell's current contents is itself a
`ContainerRef` and the value being stored is *not* itself a fresh
`ContainerRef` (i.e. this is a plain-value write through the alias, not a
new `:=` rebind of the slot), the write recurses through the nested cell
instead of overwriting the wrapper. A write of a fresh `ContainerRef` (a
genuine rebind of the alias to a different source) still replaces the
wrapper's contents, so `$alias := $other` continues to work correctly.

This was chosen over the ticket's other sketched option — replacing every
store site that holds "the alias's own cell" (env, `unit_lexicals`, VM
locals, ancestor `saved_env`/`saved_locals`) with the shared cell itself at
bind time — after auditing the call graph: **83** call sites across 29 files
funnel through `set_env_with_main_alias*`, all bottoming out in
`store_through_cell`. Fixing the one chokepoint function covers all of them
transitively; replacing the storage shape at every mint/reuse site would have
been a substantially larger, more correctness-sensitive change surface for
the same result.

Verified against real `raku` for the ticket's exact repro, a control case
confirming the already-working top-level (non-nested) bind still writes
through correctly, and a case with two independent binds to the same source
from two different subs, where both aliases observe each other's writes and
the source's writes transitively. Pinned by `t/bind-alias-reverse-write.t`
(14 subtests, all raku-verified).

As a related cleanup, the ancestor-frame propagation loop that used to be
duplicated near-identically across the `SetLocal`
(`src/vm/vm_var_assign_set_local.rs`, two branches) and `SetGlobal`
(`src/vm/vm_exec_dispatch.rs`) `:=`-bind handlers is now a single shared
helper, `Interpreter::propagate_bind_to_ancestor_frames`
(`src/vm/vm_var_assign_ops.rs`). Its `saved_locals[i]` patch is still
indexing with the wrong frame's slot layout for the general cross-function
case (confirmed still accurate — not unconditionally dead, since a
same-function recursive ancestor frame happens to share `code`'s layout);
that residual is tracked in
`todo/tickets/bind-alias-saved-locals-wrong-frame-index.md`.
