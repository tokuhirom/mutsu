# A regex `$var` atom borrows its string value

Since #16474 every plain `$var` of a regex is read where the cursor reaches it, so
`var_interp_target` runs once per start position the matcher tries. Each run built the
`$name` key with `format!` (twice when the plain name missed), and copied a `Str` value into a
fresh `String` to compare it character by character. The key is now built in a stack buffer and a
`Str` value is compared in place.

`scripts/bench-det.sh benchmarks/bench-regex-match.raku` (profiling build): 478.3M instructions /
476,052 allocations on main -> 459.8M / 399,246 (-3.9% / -16%); pre-#16474 is 424.8M / 388.4k, so
the close condition of #16614 is not met yet: what is left is the match-time scope install per
match (`install_env_scope` copies the captured names, `capture_regex_closure`). Refs #16614.
