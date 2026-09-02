# A list literal captures a scalar variable's container but not an element's

`MakeArray` gives a `List` element the SOURCE container when the element
expression is a scalar variable, so a later alias of that element reaches the
variable:

```raku
my $x = 1; my $l := ($x, 6); my \a := $l[0]; a = 10; say $x;   # 10 — mutsu and raku agree
```

The same does not happen when the element expression is an array or hash
ELEMENT, so the list holds a copy:

```
raku  -e 'my @a=1,2; my (\p,\q) := (@a[0],@a[1]); p=9; say @a'   # [9 2]
mutsu -e 'my @a=1,2; my (\p,\q) := (@a[0],@a[1]); p=9; say @a'   # Cannot modify an immutable Int (1)
mutsu -e 'my @a=1,2; my $l := (@a[0],); say $l.raku'             # (1,) — the cell is already gone here
```

## Root cause

`exec_make_array_op` (`src/vm/vm_data_ops.rs`) captures a source container only
for an element the compiler tagged with `OpCode::WrapVarRef`, and
`compile_call_arg` (`src/compiler/helpers_call_args.rs`) emits that tag from a
`source_name` computed from `Expr::Var`/`ArrayVar`/`HashVar`/`CodeVar`/
`BareWord`/`AssignExpr`/`DoStmt`. An `Expr::Index` has no source *name*, so it
gets no tag and the list stores the dereferenced value.

Making it work needs a ref-producing emission for an index expression in
list-literal position — the element-lvalue machinery
(`IndexAutovivifyLazyTerminal` / `array_slot_ref`) already exists, but it is
currently reached only from a bind/`is rw` context, not from ordinary list
construction. That is a change to what every parenthesised list holds, so it
wants its own measurement pass.

## Provenance

Found while landing `news/2026-09/list-destructuring-sigilless-bind.md`
(2026-09-02). It is the last row of that ticket's divergence work that did not
close: the destructuring desugar is correct now, but it can only alias what the
RHS list actually carries. Before that change the same program silently
no-opped; it now dies, which is at least a visible failure.

Related: `todo/deep/immutable-list-element-bind-is-writable.md`.
