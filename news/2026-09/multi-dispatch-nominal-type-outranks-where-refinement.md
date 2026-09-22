# Multi dispatch ranks nominal types before `where` refinements

`multi sub f($x where * does R2)` beat `multi sub f(Int $x)` for `f(42)`, even
though `42` does not compose `R2` and the where-clause candidate's nominal type
(an implicit `Any`) is strictly *wider* than `Int`. Rakudo answers `is-Int`;
mutsu answered `does-R2` ([#8958](https://github.com/tokuhirom/mutsu/issues/8958)).

## The flat tuple was the bug

`candidate_specificity_rank_for_args` returned a 6-tuple compared
lexicographically:

```
(literal_value_count, where_count, subset_type_count, typed_param_count,
 subsig_count, writable_trait_count)
```

`where_count` sat two slots above `typed_param_count`, so merely *having* a
`where` clause made a candidate narrower than any sibling with a narrower
nominal type — the where clause was never even evaluated, only counted. The
type-hierarchy distance (`candidate_type_distance`) was compared later still,
after the whole tuple, so the same inversion applied one level up: a candidate
constrained to `Cool` with a `where` beat one constrained to `Int` for an `Int`
argument.

Rakudo's `is_narrower` works in two tiers. It compares the candidates' nominal
parameter types first, and only consults a parameter's constraint — a `where`
clause, or the equality check a literal or a `subset` compiles to — when the
two candidates are **tied on every nominal type**. A refinement is a
tie-break, never a promotion.

## The rank key now models both tiers

`CandidateRankKey` is now explicitly tiered:

```
((literal_value_count, typed_param_count),      # nominal tier
 type_hierarchy_distance,                       # nominal tier, finer grained
 (where_count, subset_type_count, subsig_count, writable_trait_count),
 declares_named, optional_positional_count, required_named_count, decl_order)
```

A literal parameter leads the nominal tier rather than the refinement tier
because rakudo compiles `multi f(42)` to the literal's own type *plus* an
equality constraint: it is nominally exactly as narrow as the argument, and
only then wins the tie on the constraint. `native_infix_dispatch` and
`native_increment_dispatch` already ranked user candidates against core ones
this way, open-coding the tier split at their call sites; they now get it from
the shared tuple.

Hoisting the type distance above the refinement tier required three
corrections to `candidate_type_distance`, each of which had been masked by the
refinements outranking it:

- A `subset` is a refinement of another type, not a type of its own. Measuring
  `Even` (or `UInt`) against the hierarchy found it nowhere and scored the 500
  "unrelated" distance, which would have lost every tie a subset exists to win.
  `dispatch_nominal_base` now resolves a subset — through a chain of them, if
  declared that way — to the type it refines, so `subset Even of Int` ties
  `Int` on distance and wins on `subset_type_count`.
- A literal parameter carries no type constraint, so it scored the implicit-
  `Any` distance. It is nominally exactly as narrow as any argument that can
  bind to it, so it now scores 0.
- An **optional** positional the call supplied no argument for was charged a
  flat 1000, which made a trailing guard parameter lose to its own shorter
  sibling. Rakudo caps its comparison at the shorter signature's positional
  count whenever two candidates share a minimum arity, so a surplus optional
  contributes nothing to either side. The constrained branch already
  contributed nothing in this case; the two paths now agree.

## Pin

`t/routines/dispatch/multi-nominal-narrowness-beats-refinement.t`, 14
assertions verified verbatim against rakudo: untyped-`where` vs typed,
same-typed `where` vs same-typed plain (the `where` must still win), a nearer
nominal type vs a wider one carrying a `where`, literal vs its own and a wider
nominal type, `subset` / `UInt` / subset-of-a-subset vs the base type, `subset`
vs untyped-`where`, sub-signature vs bare `Positional`, the nominal count
summed across several positionals, and a trailing `where`-guard on an
unsupplied optional.
