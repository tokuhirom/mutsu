# A class accessor beats a role method for quoted method calls

`$obj."name"()` and `$obj."$name"()` on a named receiver resolved a role-composed
`method street { Str }` ahead of the class's own `has Str $.street` accessor, so
dynamic accessor reads came back empty. The compiled-mut dispatch tail now defers
to the accessor when it wins the per-level resolution race, matching the plain-name
call. Found by taking the `Contact` distribution through the ecosystem roulette:
all five of its baseline test files now pass.
