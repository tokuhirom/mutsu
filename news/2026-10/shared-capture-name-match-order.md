# A name both sides of `%` or `~` capture lists in match order

When a separated quantifier's atom and separator captured the same name
(`<x>+ % <x>`), or a `~` goal match's goal and inner pattern did
(`"(" ~ <a> <a>`), mutsu listed that name's entries side by side: every atom's
before every separator's, and the goal's before the inner pattern's. So
`<x>+ % <x>` on `"abc"` gave `a|c|b`. Rakudo lists them in match order
(`a|b|c`), and now so does mutsu (#10574).

Both regex engines fold through the same helpers: the separated quantifier's
`append_separated_captures` merges names `a0, s0, a1, s1, …`, and the new
`merge_goal_captures` puts the inner pattern's names before the goal's.
Positional slots keep their source numbering.

Filing in place already gives match order. The compiled engine therefore no
longer needs the disjointness test that kept such shapes in per-iteration
capture levels (ADR-10488 D3). A separated quantifier or goal match whose two
sides share a name, such as a `rule`'s `<.ws>` on both sides, now files
straight into its level.
