# An anonymous sub's sigilless parameters remain visible in a nested Proxy callback

An anonymous `sub` with sigilless parameters (`\\obj`, `\\key`) can return a
`Proxy` whose `STORE` callback uses those parameters. The interpreter-path
recompilation of the sub body now preserves that lexical context, so the
callback receives the original argument values instead of treating the names
as barewords.
