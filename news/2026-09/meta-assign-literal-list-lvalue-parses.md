# `($a, $b) X+= 2, 3` parses

An `X`/`Z` meta-assignment whose lvalue is a literal list of scalars parsed in
the bracketed spelling and not the unbracketed one:

```raku
my ($a, $b) = 1, 10;
($a, $b) X+= 2, 3;
```

```
===SORRY!=== Error while compiling -e
Confused. expected statement: ...
------>my ($a,$b)=1,10; ($a,$b) X+= 2,3; say "$a,$b"
```

rakudo: `6,15`. The bracketed `($a, $b) X[+=] 2, 3` already worked on mutsu
and is pinned in `meta-cross-zip-assign.t`. Raku makes no distinction between
the two spellings.

## Root cause

This is the residue of `news/2026-09/bare-meta-assign-spelling-accumulates-in-place.md`.
That entry taught the unbracketed spelling to mean "the meta-operator over the
assignment infix" rather than "the meta-operator over the plain infix, then
assign" — but it did so in `assign_stmt` (statement position) and `try_assign`
(expression position), and both of those parse a lexical variable name before
they look at the operator. A parenthesized list never reaches either.

Where a literal-list lvalue does land is the list-infix loop
(`src/parser/expr/precedence/list_infix_loop.rs`), and there
`parse_meta_compound_assign_op` was consulted only for `R`. `X` and `Z` fell
through to the general meta-operator scan, which took `X+` and left a stranded
`=` for the next parser to choke on.

## Fix

The same rewrite, in that third place: an `X`/`Z` meta-assignment over a
compound inner op becomes `MetaOp { meta, op: "<base>=" }`, which is exactly
what the bracketed spelling already produced there via `parse_meta_op`, and
what the compiler's `Expr::ArrayLiteral` branch distributes back across the
individual containers.

It is restricted to a literal-list left on purpose. A `$`/`@` variable left is
rewritten by the two sites above, whose comma-boundary logic needs
`expression()` to leave the operator alone — the same reason the bracket
branch in `precedence/logic.rs` limits itself to subscripted lvalues. The two
variable spellings are pinned in the new test so the restriction cannot
quietly lapse.

Pinned by `t/lang/operators/meta-assign-literal-list-lvalue.t`, whose twelve
assertions all pass unchanged under rakudo. Each unbracketed assertion is
paired with its bracketed twin, since the point is that the two spellings mean
the same thing.

## Still not covered

`@a[0,1] X+= 10` — a *subscripted* lvalue — remains unmutated, which
`meta_assign_writeback_target` in `src/compiler/expr_ops.rs` already records as
a `TODO` for both spellings alike. That is a compiler-side writeback gap, not a
parse one, and it is not specific to the unbracketed form.
