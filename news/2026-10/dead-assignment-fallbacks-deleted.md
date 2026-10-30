# Dead statement-level assignment fallbacks deleted (coverage round 2)

A coverage run over `t/`, the roast whitelist and a parse-only pass over roast, vendor and bundled
modules (`docs/dead-code-coverage.md`, default frontend) showed two parser blocks that no file reached:
the `($x = $y) = 5` branch of `expr_stmt` (the expression grammar already consumes the construct) and
`rewrite_scalar_assignment_{rhs,stmt}_as_sink`, whose `X`/`Z` arms never matched. Both are deleted with
their callers' branches; `t/lang/parsing/paren-assign-then-assign.t` pins the surviving behaviour, and
the output of the sink-context `X`/`Z`/`[X+]=` statements is unchanged.
