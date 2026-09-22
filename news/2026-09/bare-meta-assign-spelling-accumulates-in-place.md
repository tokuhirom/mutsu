# `@a X+= @b` accumulates into its left cells, like `@a X[+=] @b` always did

Raku writes a meta-operator over an assignment infix two ways, and they mean
the same thing: `@a X[+=] @b` and `@a X+= @b`. mutsu only implemented the
first.

```raku
my @d = 1, 2;
@d X+= (10, 20);
say @d.raku;          # mutsu: [11, 21, 12, 22]   raku: [31, 32]
```

`X` over `+=` mutates each *left* cell in place, with the left index slowest:
every right value accumulates into the first cell before the second is
touched, and the left container keeps its length. mutsu produced the flattened
cross product and grew a two-element array to four.

## Root cause

The parser lowered the unbracketed spelling as "meta-operator on the plain
infix, then assign": `@a X+= @b` became `@a = (@a X+ @b)`. The comment at the
head of the branch in `src/parser/stmt/assign/assign_stmt.rs` said so in as
many words — `// Meta-op assignment: @a X*= 10 → @a = @a X* 10`.

That is a different operator. `X+=` is `X` applied to the infix `+=`, and the
AST for it is `MetaOp { meta: "X", op: "+=" }` — exactly what the bracketed
spelling already produced, and what the compiler lowers to
`OpCode::MetaOpAssign`, whose `Cross` arm implements the in-place accumulation
correctly. The unbracketed spelling simply never reached that opcode.

**`Z+=` was broken too**, which the issue did not expect: `@a = (@a Z+ @b)`
agrees with element-wise accumulation only when both sides are the same
length, and every reported example happened to be. Where they differ, the
rewrite drops the left's trailing cells:

```raku
my @c = 1, 2, 3;
@c Z+= (10, 20);
say @c.raku;          # mutsu: [11, 22]   raku: [11, 22, 3]
```

So the zip sibling was not the working contrast it looked like — it was the
same bug, masked.

## Fix

Both spellings now build the same AST. `assign_stmt` (statement position) and
`try_assign` (expression position) rewrite an `X`/`Z` meta-assignment over a
compound inner op to `MetaOp { meta, op: "<base>=" }`, leaving the compiler's
existing `MetaOpAssign` path to do the work.

It becomes an expression statement rather than a `Stmt::Assign`: the opcode
mutates the left cells and writes the container back itself, so an outer
assignment would store the per-op result Seq over what the opcode had just
mutated. That Seq is the expression's value, and for `X` it differs from the
container — `my $v = (@f X+= (10, 20))` leaves `@f` as `[31, 32]` and `$v` as
`(11, 31, 12, 32)`, which mutsu now reproduces exactly.

Three things deliberately keep their existing lowering: `R op=`, which assigns
to its *right* operand; `Z=` (the bare `=` inner op), which is element-wise
assignment via `__mutsu_zip_assign`; and a scalar `Z op=`, which folds to the
plain compound assignment because zip over one-element operands is the base
operator. A `%`-sigil left also stays as it was — `meta_assign_writeback_target`
handles only `$` and `@`, and rakudo itself dies on `%h X+= (10, 20)`, so there
is nothing to match.

Pinned by `t/lang/operators/meta-cross-zip-assign-bare-spelling.t`, the
unbracketed twin of `meta-cross-zip-assign.t`. Its eighteen assertions all pass
unchanged under rakudo, and the uneven-length zip cases are the ones that would
catch a relapse.

## Not fixed here

`($a, $b) X+= 2, 3` — the unbracketed spelling with a *literal list* lvalue —
does not parse at all
([#9046](https://github.com/tokuhirom/mutsu/issues/9046)). That is a parse gap
rather than a lowering one: it never reaches either site changed here, both of
which parse a variable name first. The bracketed `($a, $b) X[+=] 2, 3` works
and is already pinned.
