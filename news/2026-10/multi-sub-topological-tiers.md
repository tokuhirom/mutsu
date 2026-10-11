# Multi subs rank candidates in rakudo's topological tiers

Rakudo sorts a multi's candidates into tiers. Tier 0 holds every candidate
that no other candidate is narrower than; the next tier holds those that only
tier-0 candidates are narrower than, and so on. The candidates that cannot
bind a call still shape the tiers, so two candidates that are incomparable
on their own can land in different tiers because of a third one between them.

mutsu compared only the candidates that bound, pair by pair, and fell back to
its lexicographic rank where rakudo's `is_narrower` calls a pair
incomparable. That happens when the two differ in both positional count and
required positional count and neither has a slurpy. With

```raku
multi a(Int $g, Int $e, $d = '')         { "A" }
multi a(Any $g, Any $e, Any $t, $d = '') { "B" }
multi a(Int $g, Int $e, $d = '', :$rel!) { "C" }
```

`a(5, 6, 1)` answered `A`, from its more specific types. Rakudo answers `B`:
`C` is narrower than `A`, and `C` and `B` are incomparable, so `B` shares
tier 0 with `C` while `A` sits in tier 1.

`MultiRank` now carries a `tier`, assigned over each multi-sub dispatch stage
(`multi_rank::assign_tiers`) and compared first. A binding candidate in an
earlier tier always wins (#12795). Test's `is-approx(5, 6, 1)` now reaches
its absolute-tolerance candidate this way rather than through the typed-count
tier. Multi methods still rank only the candidates that bound, so for them
the tier is 0.
