# A sigilless target of a list-destructuring bind is an alias, not a copy

```
raku  -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'   # 10
mutsu -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'
# was: Cannot assign to an immutable value       now: 10
```

The single-variable form (`my \a := $x; a = 10`) had aliased for a long time,
and so did hand-unrolling the list (`my \c := $p; my \d := $q`). Only the
parenthesised list form was broken, and it had been broken in three different
ways over time — silently no-op, `Cannot assign to a readonly variable`, and
finally `Cannot assign to an immutable value`.

## What was wrong

`parse_positional_destructuring` stages the RHS in a synthetic
`@__destructure_tmp__` and gives each target one element of it. For a sigilless
target it emitted a *value declaration plus two readonly marks*
(`VarDecl` + `MarkSigillessReadonly` + `MarkReadonly(Immutable)`), which is the
shape for `my \a = 5` — a term that IS its value — rather than the
`SyntheticBlock([MarkBind, VarDecl, MarkSigilless])` shape that
`my \a := $x` uses and that leaves writability to the runtime.

The staging temp was the other half. It was declared with a plain assignment,
which deitemizes the RHS list's element cells away
(`itemize_elements_for_var_assign` strips them for exactly this container), so
even a correct target shape would have had nothing to alias.

## The fix

Two changes in the desugar (`src/parser/stmt/decl/destructure.rs`):

* in binding mode the staging temp is declared with `MarkBind`, the same marker
  `my @t := (...)` uses, so it keeps the element containers the RHS list
  carries. Targets that read a *value* out of it are unaffected — a `$` target
  of a `:=` is a read-only copy in Raku too, and `my ($a,$b) := ($x,$y); $x = 7`
  correctly leaves `$a` at 1;
* a sigilless target emits the single-variable bind's block shape, so
  `OpCode::MarkSigillessBind` settles its mutability at run time.

That second change needed one supporting piece. A sigilless bind of a `List`
element must NOT be writable (`my (\a, \b) := (5, 6); a = 10` dies in rakudo),
but the terminal bind subscript promotes any scalar leaf to a fresh container
cell, which would make it look writable. `OpCode::IndexAutovivifyLazyTerminal`
therefore gained a `sigilless` flag, threaded from the declaration through the
compiler (`sigilless_bind_vardecl` / `sigilless_bind_terminal`), that suppresses
the promotion for an immutable `List`. An element that already IS a container —
a captured source cell from `($x, $y)`, a nested `Array`/`Hash` — is handed back
untouched and stays writable.

The flag is deliberately narrow: the same over-promotion makes
`my $x := (5,6)[0]; $x = 10` wrongly writable, but making the rule
unconditional breaks three consumers that lean on the promotion (a chunked
`for @flat -> \a, \b` loop parameter, `.kv` on a mutable QuantHash, `.kv` on a
`Pair` in a closure). Those are recorded, with the prototyped fixes for two of
them, in `todo/deep/immutable-list-element-bind-is-writable.md`.

## Relation to `4016f677c`

`4016f677c` ("fix: preserve lvalues in sigilless destructuring") landed on
`main` while this was in flight and took the originating ticket's *first*
suggestion: when every target is sigilless AND the RHS is a literal list of
plain variables or literals, skip the staging temp and emit N single binds.
That covers the headline case but bails to the old (broken) path for the rows
the ticket had already measured as the reason to reject it — a mixed
`my (\a, $b) := ($x, $y)`, and a non-literal RHS such as
`my (\a, \b) := @z`. The change here fixes the staging path itself, so it
subsumes that fast path; the fast path was removed rather than kept as a second
code path for the same shapes, and its test cases were merged into the pin.

## Result

Every row of the originating ticket's measured divergence table now matches
rakudo, including the ones that rule out a narrower fix: the RHS need not be a
literal list (`my @z = 1,2; my (\a,\b) := @z; a = 10` gives `[10 2]`), mixed
sigilless/sigilled targets work, and the alias survives into a closure.

Pinned by `t/list-destructuring-sigilless-bind.t` (16 tests). Gates: `make
test`, a full local `make roast`, and `scripts/battery-testsuite.sh` all pass.

One divergence from that table did not close and got its own ticket: a list
literal built out of array/hash *elements* (`(@a[0], @a[1])`) still stores
copies, because `MakeArray` captures a source container only for a named
variable — `todo/tickets/list-literal-does-not-capture-element-containers.md`.
