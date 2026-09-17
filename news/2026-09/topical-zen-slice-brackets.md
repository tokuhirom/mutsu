# Topical zen slice `.[]` / `.{}` (no explicit invocant) now parses

Part of the #7988 `859ab33e` parse-error-expectation-dump cluster (36 distributions
before this fix).

Raku's zen slice — an empty subscript that returns the whole container unchanged —
already worked with an explicit invocant (`@a[]`, `%h{}`) and as the angle-bracket
spellings on the implicit topic (`.<>`, `.«»`). The bracket/brace spellings on the
implicit topic (`$_[]`/`$_{}` written as a bare `.[]`/`.{}`) did not: `CSS::Nested`
0.0.1 could not even `use` its own module because of

```raku
my @rsets = %( :ruleset(%( :@selectors, :declarations(.[]) ))) with %ds<decl>;
```

where `.[]` slices the topic (here, `%ds<decl>`, a `Pair` value) into a plain list.

The "leading dot on the implicit topic" primary term (`topic_method_call` in
`src/parser/primary/regex/lit.rs`) has its own `.[index]`/`.{index}` branches, each
of which always expects at least one index expression — an immediately-closed
`.[]`/`.{}` fell through every alternative and surfaced this cluster's generic
"Confused. expected statement: ..." message.

Fixed the same way `.<>` already was: when the bracket/brace is immediately closed,
hand the bare topic back with the opener unconsumed, so the general (non-topical)
postfix layer's existing zen-slice handling — including its `:exists`/adverb forms
and `:=`/`::=` binding diagnostics — applies uniformly instead of being restated
here. `.[index]`/`.{index}` with an actual index are unchanged.

Pinned by two new cases in `t/collections/subscript/dotted-postcircumfix-subscripts.t`
(topical `.[]`, topical `.{}`, and `.{}:exists`), measured against `raku` v2026.07.

`CSS::Nested`'s only test file now runs to completion and passes on both mutsu and
rakudo, moving the distribution from `blocked_load` to `green`.
