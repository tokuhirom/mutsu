# A match inside a bare block no longer clobbers a parameter `$/`

In `sub f($/) { ...map({ $_.subst(/x/, '') })... }` the block shared the routine's `$/` through a
boxed cell, so the block's `.subst` / `~~` overwrote the routine's `$/`, and a following `make $/...`
died with "The make function expects $/ to contain a Match, but it contains Nil". A routine whose `$/`
is a parameter (a grammar action, say) now keeps it: the block binds its own `$/`, as on Rakudo.
`Markdown::Grammar`'s `t/02-Mathematica.rakutest` now passes.
