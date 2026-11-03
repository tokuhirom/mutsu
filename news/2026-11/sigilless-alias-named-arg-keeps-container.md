# A sigilless alias as a named argument keeps its container

`f :p(alias)` where `my \alias = $x` now records the alias's term key as the
named argument's source, so an `is raw` parameter (directly or through a `|c`
capture) writes through to `$x`, as in Rakudo. Before, only `$`-sigiled
variables carried a source and the write died with "Cannot assign to an
immutable value" (#16230).
