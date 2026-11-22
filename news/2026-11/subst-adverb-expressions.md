# `s///` evaluates its `:nth` and `:x` arguments as expressions

`s:nth($n)/.../.../`, `s:nth({2})///`, `s:x($n)///` and the `S///` forms used
to die with "Invalid :nth index ($n)": the parser kept the adverb argument as
raw text, and the substitution parsed that text again when it ran, which only
understood integer literals, comma lists, ranges and `*`.

The argument is now an expression in the AST (`Expr::Subst::nth` / `x`). It is
compiled and evaluated like any other argument, and its value is pushed beside
the substitution op. The VM resolves it with the resolvers `.match` and
`.subst` already use: `resolve_callable_nth` for a Callable, then
`resolve_nth_value_indices` for the indices, and `parse_match_repeat_bounds`
for `:x`. That retires the text parsers `parse_subst_nth_spec` and
`parse_subst_x_spec`, a second implementation of the same selection.

Two smaller fixes came with it. `:nth(Inf)` now selects the last match.
`.match` now reports a bad `:nth` with rakudo's messages ("Attempt to retrieve
before :1st match -- :nth(0)", "Attempt to fetch match #1 after #3"); before,
it said "Unexpected value (0) for :nth".
