# Core IO::Path candidates outrank an untyped user multi

A module-local `multi sub slurp($source where ...)` (or `dir`, `spurt`, `unlink`) with an untyped
first parameter no longer intercepts a call with an `IO::Path` argument: the setting's
`IO::Path`-typed candidate is narrower, so it wins and the user's `where` clause never runs, as in
Rakudo. Found via the Data::Importers distribution (`t/02-basic-usage-via-slurp.rakutest` now passes).
