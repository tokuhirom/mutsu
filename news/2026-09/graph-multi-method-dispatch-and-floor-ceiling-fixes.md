# Graph ecosystem distribution: inherited multi-method dispatch fixes

Working the `Graph` distribution (0.1.3, locked on
[#8977](https://github.com/tokuhirom/mutsu/issues/8977) via
`ecosystem-dist-roulette`) surfaced two real, general-purpose multi-method
dispatch bugs, both now fixed, taking the distribution's baseline from
16/24 files at parity to 18/24.

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

## floor()/ceiling() free functions: tried, reverted, filed instead

The `.floor`/`.ceiling` *method* forms already return `Int` correctly; the
free-*function* forms (`floor(x)`/`ceiling(x)`) return `Num`. A
`UInt:D`-typed named parameter fed such a result then fails its own type
check — `Graph::MinCuttish.find-minimum-cut(method => 'karger-stein')`
computes its `UInt:D :$th` from `ceiling(1 + $n / sqrt(2))`, and the
resulting type-check failure on the sole (non-multi) private method
`!karger-contract` is misreported as "No matching candidates for method:
karger-contract".

Changing the free-function form to always return `Int` (matching the method
form) initially looked like the fix, and made `t/17-find-minimum-cut.rakutest`
pass — but it regressed the already-whitelisted `roast/S02-types/num.t`,
whose `'ceiling(num)'` subtest pins that a genuinely native `num`
(lowercase, unboxed) scalar keeps `ceiling()`'s result as `Num`, unlike a
boxed `Num`/`Rat`. mutsu has no runtime representation distinguishing the
two (`my num $y` and `my Num $y` report the identical `(Scalar)` for
`.VAR.WHAT`), so there is no way to pick the right answer at the point
`floor`/`ceiling` currently dispatch from. Reverted, and filed as
[#9012](https://github.com/tokuhirom/mutsu/issues/9012) — the real fix needs
the argument's declared/static nativity plumbed through to the builtin
dispatcher, a cross-cutting call-site change rather than a one-file fix.

## Remaining red files

Six findings that didn't fit a bounded fix in this pass were filed as
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
- [#9012](https://github.com/tokuhirom/mutsu/issues/9012) — `floor()`/
  `ceiling()` free functions can't distinguish native `num` (keeps `Num`)
  from boxed `Num`/`Rat` (returns `Int`) — no runtime nativity tag
  (`t/17-find-minimum-cut.rakutest`).
