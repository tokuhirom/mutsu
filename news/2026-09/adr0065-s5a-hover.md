# Hover tells you whether mutsu has the thing you are hovering

ADR-0065 scheduled S5 as one slice, "`references` / `hover`". Splitting it was
the first thing S4 made obvious: `hover` is `definition`'s machinery with a
different answer attached — position → identifier → symbol — and needs no spans
at all, while `references` is the *only* method that genuinely does. Shipping
them together would have held a cheap, useful method behind the heaviest
engineering left in the plan.

So `hover` ships now, and `references` becomes S5b.

## Signatures are reconstructed from the parse, not lifted from the source

Hovering `add` shows `sub add(Int $a, Int $b --> Int)`, rendered from the parsed
`ParamDef`s back into Raku source form. Copying the declaration's source text
would have been easier and is the wrong answer: what a writer targeting mutsu
needs to see is **what mutsu understood the signature to be**, which is exactly
where a divergence from rakudo would show up.

The rendering is deliberately partial. `where` clauses, sub-signatures and
default *expressions* are dropped, because rendering an expression back to source
needs a printer mutsu does not have and a half-rendered default would be a hover
that lies. A default shows as `= ...`, which says there is one without claiming
what it is.

Two AST details had to be recovered rather than assumed, and the tests caught
both immediately: `ParamDef::name` stores `@rest` for an array but a bare `a` for
`$a` — the scalar sigil is stripped at parse time — and `required` only ever
means "a named parameter written with `!`". A mandatory positional carries no
flag at all, so positional optionality reads `optional_marker` instead. The first
draft rendered `(Int a?, Int b?) --> Int` for `(Int $a, Int $b --> Int)`: wrong
sigils, wrong optionality, and the return type outside the parentheses.

## "mutsu implements this" is worth saying out loud

The obvious design reports only the negative: hovering a routine mutsu lacks says
so, with mutsu's own "Did you mean" attached. The affirmative case is reported
too — hovering `uc` says it is a built-in and that **mutsu implements it**.

That is not padding. To this consumer, silence is indistinguishable from "the
server did not understand the question", and "mutsu has this" is precisely what
someone writing Raku for mutsu wants confirmed. It is the same D4 signal as S2's
diagnostic, delivered where a reader is already looking and before the code is
ever run.

## What is left

`references` (S5b) is the one method that cannot be done this way. `definition`
gets away without positions because it only needs to know what *one* word is;
`references` must find every occurrence and rank them, which text scanning cannot
do soundly. It is where D6's "spans only on the variants a feature demands"
finally gets exercised — and where the parser's hot path and the bincode AST
precompilation cache come into range, so the target variants want settling before
any code is written.
