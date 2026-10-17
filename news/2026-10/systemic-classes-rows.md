# The `Systemic` classes' methods are method rows

`$*DISTRO`, `$*KERNEL`, `$*VM`, `$*RAKU` and `$*RAKU.compiler` answered their methods from
by-name `match`es, with a hand-written list per class saying which names were native. Their
methods are now method-table rows, and the per-class name list is derived from the rows. A method
the class does not declare is `Mu`'s, so `$*VM.raku` and `$*RAKU.compiler.raku` render the
object's attributes as Rakudo does (`VM.new(config => ..., ...)`), where they used to print
`mutsu.new(...)` (ADR-11276 §9.155).
