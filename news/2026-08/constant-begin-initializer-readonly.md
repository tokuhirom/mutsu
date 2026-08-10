# `constant X = BEGIN ...` no longer dies "Cannot assign to a readonly variable"

```raku
constant E = BEGIN 5;        # was: Cannot assign to a readonly variable (E) or a value
my constant F = BEGIN 5;     # same
constant G = BEGIN { 5 };    # same
```

raku accepts all three (`say E` prints 5). The root cause was in the
phaser-reordering pass (`reorder_at_level`, `src/runtime/phasers.rs`), not in
the `BEGIN`-compilation code the original ticket pointed at. Any block
containing a nested `BEGIN`/`CHECK`/`INIT` phaser expression anywhere (here,
inside the `constant`'s own initializer) triggers a whole-block reorder: every
`VarDecl` in that block is split into a bare hoisted declaration (so
`CHECK`/`INIT` phasers can see variables declared later in source order) plus
a separate `Assign` at its original position.

The compiler marks a `constant`'s local slot readonly *unconditionally* at
the end of compiling its `Stmt::VarDecl`, regardless of whether an
initializer is present. So the bare hoisted `constant E;` was already
readonly by the time the split-out `E = ...` assign ran — and unlike a
`VarDecl`'s own store (guarded by `MarkVarDeclContext`, which lets a
declaration overwrite the just-created binding), a plain `Stmt::Assign` does
not bypass the readonly check.

Fixed by excluding `constant` declarations from this split entirely: a
`constant` is now always compiled as one unsplit statement (its normal,
unhoisted path), keeping the readonly mark strictly after its one real store
— the same shape a phaser-free `constant` already used.

While testing, two adjacent, pre-existing bugs surfaced and were filed
separately rather than folded into this fix:
- `todo/tickets/begin-value-position-does-not-see-a-prior-constant-in-an-expression.md`
  — `BEGIN A + 1` (referencing a prior `constant A` inside an expression)
  reads `A` as a bareword string instead of its constant value.
- `todo/tickets/statement-level-begin-side-effects-lost-with-later-vardecl-splits.md`
  — a statement-level `BEGIN { @order.push(...) }` block's side effect on an
  outer array is silently lost when the same statement list also contains a
  later `VarDecl` that triggers this same reordering split (reproduces with
  plain `my`, not just `constant`).

Regression test: `t/constant-begin-initializer.t`.
