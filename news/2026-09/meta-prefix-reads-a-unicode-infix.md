# `@a Z× @b` parses: a meta prefix can read a Unicode infix

A Unicode operator alias parsed as a plain infix (`1 × 2`), inside a reduction
(`[×] @a`) and inside hyper delimiters (`@a >>×<< @b`) — but not after a
`Z`/`X`/`R` meta prefix, where the parser stopped dead at the non-ASCII byte:

```raku
my @a = 1, 2;
my @b = 3, 4;
say (@a Z× @b).raku;
```

```
===SORRY!=== Error while compiling -e
Confused. expected statement: ...
------>my @a=1,2; my @b=3,4; say (@a Z× @b).raku
```

rakudo: `(3, 8).Seq`.

## Root cause

Two halves, both of them the same drift: the ASCII table and the alias table
were maintained separately, and the alias table was only consulted at run
time.

**The scanner.** `parse_meta_op`
(`src/parser/expr/precedence_meta_ops/meta_bracket.rs`) matches the inner
operator against a hand-written list of symbolic spellings. That list is
ASCII-only. The runtime side was already in place — `canonical_infix` folds
`×` to `*`, and `InfixShape::lower("Z×")` decodes to a `Zip` layer over a `*`
leaf — but nothing could produce a `Z×` for it to lower, because the parser
never got past the `×`.

**The reduction.** `[Z×]` parsed (the reduction bracket has its own scanner)
and then died `Unknown function: infix:<Z×>`, while the identical `[Z*]`
worked. `is_builtin_infix` recurses through the `R`/`Z`/`X` prefixes and then
looks the leaf up in a table that has no aliases in it. The bare `[×]` escaped
this only because `ReductionSpec::decode` folds the *whole* spelling before
the lookup, which leaves the alias untouched the moment a meta prefix sits in
front of it.

## Fix

The aliases are one table now, `UNICODE_INFIX_ALIASES` in
`src/compiled_operator.rs`, with two readers that have to agree:
`canonical_infix` resolves an alias at run time, and the parser's meta-operator
scanner recognizes the same spellings so the alias can be read at all. Adding
an alias there now reaches both. Set operators (`∪`, `∩`, `⊍`, ...) stay out of
it: they are not aliases of an ASCII infix, and both sides already pair them
separately.

`parse_meta_op` scans the alias table after its ASCII table, so an alias can
never shadow a longer ASCII spelling, and it keeps the alias verbatim in the
op it returns — folding it there would make `Z×` indistinguishable from `Z*`
in the AST, and the fold belongs to `canonical_infix` at lowering time.
`is_builtin_infix` folds before its table lookup, which is what lets the
recursion answer for `Z×`.

`Z`, `X` and `R` are all fixed by this, for every alias (`×`, `÷`, `−`, `≤`,
`≥`, `≠`, `∘`), in the infix, meta-assignment (`Z×=`) and reduce-over-meta
(`[Z×]`) positions.

Pinned by `t/lang/operators/meta-prefix-unicode-infix.t`, whose twenty
assertions all pass unchanged under rakudo. Each Unicode assertion has its
ASCII twin checked beside it, because the rule being pinned is that an alias
works wherever its ASCII spelling does.

## Not fixed here

`Zo` / `Z∘` — function composition as the inner op of a meta-operator — now
parses and then fails at evaluation with `Unsupported reduction operator: o`
([#9050](https://github.com/tokuhirom/mutsu/issues/9050)). The ASCII `Zo`
fails identically, so it is not an alias problem: the meta-op evaluator has no
arm for composition. The alias fold is doing its job and handing it a correct
`o` leaf.
