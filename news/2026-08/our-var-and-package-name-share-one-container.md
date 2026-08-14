# A plain `our $x` and its package-qualified name now share ONE container

An `our` variable *is* the package-stash entry; the lexical name is only an
alias for the same container. mutsu instead stored the value twice — once
under the bare/lexical name and once under the package-qualified one — kept
"in sync" by two best-effort helpers (`sync_our_local_from_qualified` /
`sync_our_package_var_from_local`) keyed on a compile-time
`CompiledCode::our_locals` list. That list is empty for a mainline (file-
scope) `our $x` (there is no local slot for the sync to key on there — or
rather, the slot exists but is recorded under whatever
`Compiler::qualify_variable_name` returns, which at `GLOBAL` scope collapses
to the bare name, not `"GLOBAL::x"`), and is scoped to ONE compilation unit,
so a qualified write performed by a *different* `CompiledCode` — most
notably an `EVAL`'d one — never touched the declaring unit's sync list at
all:

```raku
our $a = 1;  $GLOBAL::a = 5;   say "$a / $GLOBAL::a";   # raku: 5 / 5   mutsu (before): 1 / 5
our $b = 1;  $GLOBAL::b++;     say "$b / $GLOBAL::b";   # raku: 2 / 2   mutsu (before): 1 / 2
our $c = 1;  $GLOBAL::c += 1;  say "$c / $GLOBAL::c";   # raku: 2 / 2   mutsu (before): 1 / 2
```

This blocked `roast/S02-names/our.t` test 10 (`EVAL 'class RT69460 {
$GLOBAL::rt69460++ }'` failed to update the enclosing `our $rt69460`), and was
tracked in `todo/tickets/our-var-and-its-package-name-are-two-slots.md`.

## The fix: one shared `ContainerRef` cell, not a sync

A plain untyped scalar `our $x = <expr>` (no `:=` bind, no type constraint, no
`@`/`%`/`&` sigil, not a `constant`) now compiles to a single new opcode,
`OpCode::DeclareOurScalar`, which installs ONE `ContainerRef` cell and stores a
clone of it under every name this package variable is addressed by: the
lexical local slot, the bare env key, the package-qualified key, and — since
`qualify_variable_name` collapses to the bare name at `GLOBAL` scope — an
explicit `"GLOBAL::<name>"` key too, so the literal `$GLOBAL::x` spelling
resolves to the same cell regardless of which compilation unit writes it.

From there, no bespoke sync code is needed: `GetLocal`/`SetLocal` (the lexical
name) and `GetGlobal`/`SetGlobal` (the qualified name) already have generic
`ContainerRef` read-deref / write-through logic — the same chokepoints `state`
variables and captured-closure locals already share a cell through. Since
`env`/`our_vars` are Interpreter-level state (not per-compilation-unit), a
`$GLOBAL::x` write compiled by a *later* `EVAL` — a fresh `CompiledCode` with
no knowledge of the original declaration's `our_locals` — still lands on the
exact same cell. Every other `our` shape (typed, container-sigiled,
`constant`, `:=` bound, or shadowing an outer `constant`) is untouched: it
keeps the old two-store `Dup; SetLocal(slot); SetGlobal(qualified)` sequence,
which the (now largely dormant, but still-present as a defensive fallback)
`our_locals` sync helpers continue to cover.

Two other name-based variable-store opcodes turned out to share the same
"blind overwrite" shape and needed the identical write-through fix:
`OpCode::SymbolicDerefStore` (`$::('x') = v`) and
`OpCode::IndirectTypeLookupStore` (`::('$x') = v`) both used to
unconditionally `env.insert()` the new value, silently breaking the cell
sharing for a bare-name symbolic write to an `our` scalar
(`t/symbolic-deref-assign-expr.t` test 5, caught while verifying this change,
not part of the original ticket).

## Verification

- `roast/S02-names/our.t` now passes in full (10/10, including test 10).
- New regression test: `t/our-package-qualified-shared-cell.t` — mainline
  `our` + a package-qualified `=`/`++`/`+=` write, the reverse direction
  (writing through the bare lexical name), and the EVAL-from-a-different-
  compilation-unit shape roast test 10 exercises.
- `t/symbolic-deref-assign-expr.t` (pre-existing) still passes after the
  `SymbolicDerefStore`/`IndirectTypeLookupStore` fix.
