# The parser records every parenthesization, so `.AST` renders `Circumfix::Parentheses`

`todo/tickets/rakuast-parentheses-not-preserved.md` reported that mutsu's
`.AST` never emits a `Circumfix::Parentheses` node: rakudo models any `(...)` as
`Circumfix::Parentheses(SemiList(...))`, and mutsu rendered the operand bare.
The ticket had already established that this was not the initializer's fault —
`Expr::Grouped` exists, and `rakuast/convert.rs` renders it correctly; the
parser simply never produced one, because `paren.rs` wrapped only a narrow
allowlist of shapes (junction operands, a lone `BareWord`, a `Feed`, a scalar
`Var`, an assignment, `X`/`Z` meta-ops, plus a Whatever-currying freeze). Each
entry existed because one specific downstream consumer needed it.

The ticket listed three possible fixes and asked for a measurement pass before
choosing between them, because option (1) — wrap unconditionally, then fix every
consumer that breaks — was expected to "surface every place that does not
[unwrap], as roast failures spread across unrelated synopses".

## The measurement, and what it decided

Wrapping unconditionally and running the whole `t/` suite gave a number rather
than a fear: **31 failing files out of 3698**, and a further **5 whitelisted
roast files** once the local roast run was added. Collapsing the allowlist so the
marker is added in exactly one place (`mark_parenthesized`, on the way out of
`paren_expr`) and then fixing consumers took both to zero.

The roast half is worth stating separately, because it is where the *interesting*
consumers were. `t/` found the mechanical ones; roast found the ones where a
missing peel produced a plausible-looking wrong answer rather than a crash.

Option (1) was therefore the right one, and it is now implemented. The rule the
codebase follows afterwards is a single sentence: **a parenthesization is a
property of the source, not of whichever consumer happens to care about it.** A
consumer that pattern-matches a *shape* asks for it through the new
`Expr::peel_parens()`; a consumer that genuinely cares whether parentheses were
written (junction chain flattening, list assignment, the Whatever freeze) reads
the marker directly, as it always did.

## The consumers, and the three that were worth the trip

Most were mechanical: a `match` on the operand shape that had to look through
the marker first — the hyper meta-assignment write-back, the `X`/`Z` meta-op
operands, postfix and prefix `++`/`--`, `has @.x = (1, 2, 3)`'s list default,
the enum variant list, the `for` loop's per-slot write-back sources, the
`{*}` proto-dispatch rewrite, the undeclared-name scan for `enum E (Foo, Bar)`,
and the bind-immutability predicate behind `(1, 2)[0] := 3`. Three are worth
recording in more detail.

**The Whatever-currying freeze was encoded in a way only an allowlist could
support, and the source text was the oracle.** raku distinguishes `(*)` (a curry
point: `(*).abs` is a `WhateverCode`) from `((*))` (a frozen `Whatever` *value*:
`((*)).abs` calls `.abs` on the literal `Whatever` and throws), and equally
`(*.flip)` (composes into an enclosing curry) from `((*.flip))` (a finished
`WhateverCode` you can call `.assuming` on). mutsu encoded that as "a `Grouped`
around a priming point means frozen" — which works only while `(*)` is *not*
wrapped, i.e. only while the allowlist exists. Planting that layer needed
`is_single_paren_group()`, a function that re-scanned the **source text** for a
balanced paren group; `comparison.rs` re-read the consumed LHS/RHS text for the
same reason, twice.

The freeze is now the layer *count*: `(*)` is one `Grouped`, `((*))` is two, and
`is_frozen_whatever()` reads that back. Both source-text probes are gone. The
cost is that the ~7 sites asking "is this operand a bare `*`?" had to route
through one predicate (`is_whatever_operand`) instead of matching
`Expr::Whatever` inline — and finding them was the work, because each one that
was missed produced a *different* wrong answer: `Mu ~~ (*)` became `False`,
`(**).WHAT` became `WhateverCode`, `((* quack *) quack *)` lost two of its three
parameters.

Measured against raku, all of these agree in both implementations: `(* + 1)` and
`((* + 1))` are `WhateverCode`; `(*)` and `((*))` are `Whatever`; `(**)` and
`((**))` are `HyperWhatever`; `(*.so)` and `((*.so))` are `WhateverCode`;
`((*.flip)).assuming(42)()` is `24`; `(* - 1) - 1` composes to a `WhateverCode`
that returns 4 for 6; `* xx 2` is `(*, *).Seq`; `Mu ~~ (*)` is `True`.

**A real bug fell out, and only the battery gate would have caught it.**
`Digest::SHA2`'s `sha256` returned the wrong digest for the FIPS 180-2 vector.
The cause was not the parenthesization change *per se* — it was that
`(state buf32 $w .= new)[$j] = ...`, a subscript-assign whose target is a
*declaration in expression position*, silently dropped the write once the
declaration arrived wrapped. `make test`'s own suite and roast both said
nothing; `t/digest-battery.t` is what failed, exactly as
`docs/batteries/testsuite-gate.md` claims.

**A phaser's timing depended on whether you wrote parentheses.** The phaser-lift
walker (`runtime/phasers.rs`) did not descend through the marker, so
`(gather for 1..3 { INIT take "OH"; take $_ })` ran its `INIT` once per iteration
inside the `gather` — yielding `(OH 1 OH 2 OH 3)` where raku dies with "take
without gather", because `INIT` runs at initialisation time, outside any
`gather`. Descending fixes it, with one exception that had to be measured rather
than assumed: a phaser that *is* the parenthesized expression stays where it was
written, so `is (BEGIN A + 1), 4` still sees the `constant A` declared above it.

**Container identity through a parenthesized list.** `resolve_container_var_name`
resolves `($foo, "x", 17)[0, 1][0]` back to `$foo`'s container so `=:=` can
compare cells; it matched `Expr::ArrayLiteral` inline and therefore stopped at
the marker, silently falling through to a value comparison that answers `False`.
Its `but`-tuple sibling (`True but (1, "x")`) and the method-lvalue write-back
name (`(my $overwrite = $s).substr-rw(0, $c.chars) = $c`) were the same shape of
mistake.

**Two rows had been relying on the marker's absence for the right answer.**
`enum E (Foo, Bar)` must be `X::Undeclared::Symbols` (only the `<...>` word-list
form autoquotes), and `(List)[0] := 1` must be `X::Bind`. Both used to reach the
correct outcome because the wrapped shape failed to match a more permissive arm.
Both now say so deliberately: the undeclared-name scan descends through the
marker, and the index-assign peel is restricted to the declaration shape it was
added for rather than applied to every target.

## What is pinned, and what is left

`t/paren-recorded-in-ast.t` (33 tests, byte-identical under `mutsu` and `raku`)
covers the rendering rows, the transparency rows, every lvalue spelling written
inside parentheses, the enum body, and the Whatever-freeze rows. Every row that
distinguishes `(X)` from `((X))` is now a statement about the AST rather than
about the source text, which is what makes them cheap to assert.

There is no carve-out left: `Q{(*)}.AST` renders a `Circumfix::Parentheses` like
every other parenthesized term.
