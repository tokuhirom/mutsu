# `is raw`/`is rw` parameters now alias the caller's real container, not a fresh disconnected cell

An `is raw`/`is rw` closure or sub parameter that got captured by a
`WrapVarRef`-consuming site (`key => $x`, `Pair.new(..., $x)`) inside the
callee bound to a fresh, independent `ContainerRef` cell instead of the
CALLER's actual variable container. This is ADR-0032 §2.1's probe `O`
(explicitly out of scope for that ADR, filed as its own ticket):

```raku
{ my $v = 1; my $mk = -> $x is raw { key => $x }; my $p = $mk($v); $v = 2; say $p.value }
# raku: 2   mutsu (before this fix): 1
```

## Root cause

`bind_function_args_values` (`src/runtime/types/binding_signature.rs`) already
had the right idea for `is raw`/`is rw` scalar params: box the caller's
argument into a shared `ContainerRef` cell at bind time and install it in
both the callee's parameter slot (`x`) and — via the caller's own local slot
resync (`apply_rw_bindings_to_env` / `apply_pending_rw_writeback`) — the
caller's variable slot (`v`) too. That part worked: `rust-gdb` confirmed the
caller's `$v` slot held the shared cell by the time `$v = 2` executed, and
that assignment correctly wrote through it.

The break was one step earlier, inside the callee's own body. When `key =>
$x` runs, the `WrapVarRef` op's consumer (`pop_pair_operands_capturing` →
`capture_var_cell`) calls `capture_var_cell_inner`
(`src/vm/vm_data_ops.rs`) to recover `$x`'s container. That function first
resolves `$x` through `resolve_alias_root`, which follows a
`__mutsu_sigilless_alias::x` env entry — an entry `bind_function_args_values`
*also* installs unconditionally on every `is raw`/`is rw` bind, so that a
later `:=` performed through the param (`my $y := $x`) can transitively
resolve to the caller's variable. That entry points `x` to `v` — the
caller's source name — which is not a local of the callee's frame at all.
Once the alias redirected the lookup from `x` to `v`, the compiler-baked
`slot_hint` (which correctly names `x`'s own slot) was discarded because it
no longer matched the redirected name, and the subsequent by-name search for
`v` inside `code.locals` failed. `capture_var_cell_inner` fell through to
its "not a local of this frame" branch and boxed a brand-new, disconnected
cell from the (already-dereferenced) plain value on the stack — exactly the
"found a slot" branch ADR-0032 §2.1 described, just one layer earlier than
that description implied (the false "found a slot" search key was `v`, not
`x`).

Confirmed step by step with `rust-gdb -batch` breakpoints per CLAUDE.md's
debugging guidance (no printf debugging, no rebuild-per-hypothesis): the
call-time bind correctly created and installed the shared cell at `x`'s slot
and at the caller's `v` slot; the write-through of `$v = 2` correctly wrote
through that cell; but `capture_var_cell_inner`'s alias-root resolution threw
that discovery away and re-derived (incorrectly) a fresh cell from scratch.

## Fix

`capture_var_cell_inner` (`src/vm/vm_data_ops.rs`) now checks the ORIGINAL
name's own slot (via `slot_hint`, before any `:=`-alias-root redirection)
first: if that slot already holds a `ContainerRef`, it is unambiguously the
right cell to reuse, regardless of what the sigilless-alias entry says. This
is purely additive — it only short-circuits when a slot is *already* boxed,
so it can never create a new cell that would not otherwise have been
created, and cannot change behavior for the pre-existing `:=`-alias-root
scenario the following lookup exists for (a local that isn't boxed yet).

## Tests

New file `t/is-raw-param-container-identity.t` (14 assertions) covers: probe
`O` verbatim (pointy block, `is raw`), the `is rw` sibling, write-through the
other direction, the named-sub form of both traits (same bind path), the
`Pair.new` consumer alongside the fat-arrow one, two surrounding-correctness
controls for a parameter that is *never* captured (`is raw`/`is rw` params
whose value is only read/written directly, not boxed via any `WrapVarRef`
site), a plain (non-raw, non-rw) parameter control confirming it still
snapshots the argument and stays readonly (dies on assignment, matching
Raku), and a two-calls-over-the-same-variable control ruling out any
stale-cell cross-talk between separate invocations.

All 18 existing container-capture/cell-sharing mechanism pins (the ADR-0032
sibling `t/closure-container-capture-alias.t` and its 17 neighbors) and all
35 existing rw-param/sigilless-param test files stayed green — the fix does
not touch the widely-shared param-bind fast paths for the common case where
no cell is ever created.
