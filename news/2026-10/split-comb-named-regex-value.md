# `.split(&named-regex)` and `.comb(&named-regex)` use the regex

A named regex or token passed as a value (`"a b".split(&ws)`, `$s.comb(&token)`) is a
`Routine`. `.subst` and `.match` already read its source, but `.split` and `.comb` stringified
the routine (`sub ws ($arg0) ...`) and found nothing, so `split` returned the whole string and
`comb` returned an empty list. The lookup now lives in one helper, `named_regex_routine_pattern`,
shared by `.subst`, `.split` and `.comb`. MCP::Client's SSE parser splits its buffer on
`&event-boundary`.
