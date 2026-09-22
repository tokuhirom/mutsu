# Graph ecosystem distribution: multi-method dispatch and floor/ceiling fixes

Working the `Graph` distribution (0.1.3, locked on
[#8977](https://github.com/tokuhirom/mutsu/issues/8977) via
`ecosystem-dist-roulette`) surfaced three real, general-purpose interpreter
bugs, all now fixed, taking the distribution's baseline from 16/24 files at
parity to 19/24.

## Inherited multi method dispatch mismatched named-param narrowness

`pick_method_winner`'s narrowness tie-break counted a sigilled *named*
parameter (`:%stuff`, `:@vertexes`) the same as a sigilled *positional* one.
The sigil-implies-Positional/Associative-constraint heuristic is only
meaningful for positional params — a named parameter's type decides only
whether a candidate is *applicable*, never how narrow it is, exactly as
`candidate_specificity_rank_for_args` already documents for multi-sub
dispatch. This let an ancestor class's `new` candidate (with no matching
named parameter at all, but an unrelated sigilled one) out-rank a
more-derived, exactly-matching candidate — `Graph::Circulant.new(n => 10,
jump => 4)` recursing into its own positional `new` was silently redirected
into `Graph.new(:%adjacency-map, ...)`, which then failed `BUILD` with
"Required named parameter '!n' not passed".

## Named-parameter aliases only matched one level deep

`ParamDef::named_external_keys()` (used by multi-candidate *matching*, not
binding, which already handled this) only walked one level of a named
parameter's alias chain. A named param can alias to another alias
arbitrarily deep (`:leaves(:rays(:$n))`, `Graph::Star`'s own constructor
signature, is two levels), so a call spelled with the innermost alias
(`n => 5`, the distribution's own call site) never matched the signature at
all, and fell through to an ancestor's `new` — silently constructing a bare
`Graph` missing its own required `$.n` attribute.

Both fixed in mutsu, pinned by `t/oo/method/multi-method-inherited-sigil-narrowness.t`
and `t/oo/method/multi-method-nested-named-alias.t`.

## floor()/ceiling() free functions returned Num instead of Int

The `.floor`/`.ceiling` *method* forms already returned `Int` correctly; the
free-*function* forms (`floor(x)`/`ceiling(x)`) wrapped the rounded value
back in a `Num`. A `UInt:D`/`Int:D`-typed named parameter fed the result
then failed its own type check downstream —
`Graph::MinCuttish.find-minimum-cut(method => 'karger-stein')` computes its
`UInt:D :$th` from `ceiling(1 + $n / sqrt(2))`, and the resulting type-check
failure on the sole (non-multi) private method `!karger-contract` was
misreported as "No matching candidates for method: karger-contract".

Pinned by `t/types/numeric/floor-ceiling-function-returns-int.t`.

## Remaining red files

Five findings that didn't fit a bounded fix in this pass were filed as
`tokuhirom/mutsu` issues rather than folded into this PR:

- [#9005](https://github.com/tokuhirom/mutsu/issues/9005) — `.=` does not
  write back through a method-call (accessor) target
  (`t/01-basic-usage.rakutest`).
- [#9006](https://github.com/tokuhirom/mutsu/issues/9006) — a hash bound by
  a `for X.kv -> $k, %v` signature is itemized, so a nested `for %v { }`
  iterates it as one element instead of its pairs
  (`t/24-vertex-component.rakutest`).
- [#9007](https://github.com/tokuhirom/mutsu/issues/9007) — a pure,
  clone-based constructor method (`Graph.directed-graph()`) mutates its own
  receiver instead of only the new object it returns
  (`t/10-basic-conversions.rakutest`).
- [#9008](https://github.com/tokuhirom/mutsu/issues/9008) — a recursive
  lexical `sub` inside a role's private method loses its own name under
  `Graph::Componentish`'s Tarjan SCC (`t/22-leaper-graph.rakutest`).
- [#9009](https://github.com/tokuhirom/mutsu/issues/9009) —
  `weakly-connected-components` returns components in the wrong order,
  likely a Hash-iteration-order mismatch (`t/06-weak-connectivity.rakutest`).
