# `m//` evaluates its `:nth` and `:x` arguments as expressions

The follow-up to #18816 for the match form. `m:x($n)/b/` and `m:x(1..2)/b/`
matched only once, because the parser kept `:x` only when it was an integer
literal. `m:nth({2})/b/` died "Invalid :nth index", because `:nth` was kept as
raw text and evaluated at match time by a third `:nth` implementation
(`resolve_nth_indices`, `parse_nth_junction`), which looked variables up by
name.

Both arguments are now expressions on `Expr::MatchRegexDynamicAdverbs`, next
to the `:pos(EXPR)` / `:continue(EXPR)` ones. They are evaluated at every
match and patched onto the regex value by the new `PatchRegexAdverbNth` /
`PatchRegexAdverbX` ops. The match resolves them with two helpers that `s///`
now shares: `nth_selection` (which wraps `resolve_callable_nth` and
`resolve_nth_value_indices`) and `match_x_bounds`. A Junction `:nth` (`m:nth(1|3)`)
autothreads over those values instead of splitting the source text on `|`. The text
resolvers are deleted, along with the pass that kept every local named in an
`:nth` argument synced to the env.
