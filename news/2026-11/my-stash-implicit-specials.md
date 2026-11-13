# `MY::` of a routine or mainline lists `$!`, `$/` and `$¢`

Every routine and every compilation unit's mainline declares `$/`, `$!` and
`$¢` in its outermost frame, and rakudo's `MY::` lists them there (a bare
block, pointy block or loop body does not). mutsu's compiled lexical
pseudo-stash left them out, so code walking `MY::.pairs` never met their
`Nil` values. The Rakudo-Type-Introspection distribution builds its type
table that way and reaches `Nil`, and through it `Cool`, only via the
mainline's `$!` / `$/`: its `t/02-pseudostash.rakutest` now passes 9/9 like
rakudo (it is still slow; the per-lookup cost of `CORE::<v6c>.WHO<Name>` is
tracked separately).
