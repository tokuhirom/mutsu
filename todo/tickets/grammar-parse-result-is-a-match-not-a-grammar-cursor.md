# `G.parse(...)` returns a `Match`, where raku returns a `G` cursor

In raku, `Grammar` is a `Match` subclass and a grammar's parse result IS a
cursor of that grammar, so its type reports the grammar's own name. mutsu
returns a plain `Match`.

## Repro

```raku
grammar G { token TOP { <a> }; token a { \w+ } }
say G.parse("hello").WHAT;    # raku: (G)      mutsu: (Match)
say G.parse("hello").^name;   # raku: G        mutsu: Match
say ~G.parse("hello");        # both: hello
```

The same divergence shows up on a grammar that declares attributes: raku's
`G.parse(...).invalid` answers the (undefined) attribute, mutsu dies with
`No such method 'invalid' for invocant of type 'Match'`.

## Where it comes from

mutsu represents every match as `Instance("Match")` /
`ValueRepr::Match` (`src/value/match_lazy.rs` hardcodes `match_class_symbol()`),
and `Grammar.parse` hands that value back unchanged. Making the top-level result
report the grammar's type means either giving the Match node a per-parse class
symbol, or giving grammar classes an MRO that includes `Match` and returning a
grammar instance that answers every `Match` method.

## Why it was split out

Found while fixing
[grammar-embedded-custom-assertion-method-self-type-object](../../news/2026-08/grammar-embedded-custom-assertion-method-self-type-object.md)
(2026-08-26). That ticket's two repros both asserted `.WHAT` is `(G)`; the
*reported* bugs (an attribute write dying on a type-object `self`, and the parse
returning `Nil`) are fixed, and the assertion invocant is now a real grammar
instance — but the parse RESULT's identity is a separate representation change
with a much wider blast radius (`isa-ok $m, Match`, every `.^name` on a parse
result, the `Match`-typed native method table) and did not belong in that fix.

Check `raku`'s exact contract before implementing: `Grammar.^mro` there is
`(Grammar Match Capture Cool Any Mu)`, so a grammar cursor must satisfy
`~~ Match` as well as `~~ G`.
