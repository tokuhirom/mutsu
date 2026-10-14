# RakuAST: constant initializers and `~~ ($_)` round-trip

Two lowering fixes found by the `MUTSU_RAKUAST=1` survey. A `constant`
initializer no longer marks the terms it reads (`pi`, `tau`, `e`) as declared
by the unit, so `constant p = pi` keeps the setting constant instead of the
bare word. A parenthesised topic on the right of `~~` keeps its `Grouped`
wrapper, so `"ab" ~~ ($_)` still returns a `Bool`. The ratchet also picks up
files that already passed on the refreshed tree.
