# Setting terms resolve in the RakuAST unit tree

In the unit tree a CHECK phaser reads (`$*CU`), a setting term written as a
bareword now resolves as on rakudo 2026.09: `pi`, `e`, `tau`, `i`, `Empty`,
`IterationEnd` (`RakuAST::Term::Name`) and `True`, `Less`, `Kept`
(`RakuAST::Term::Enum`) resolve to a `RakuAST::Declaration::External::Setting`
whose `lexical-name` is the term. Before, a `Term::Name` resolved to nothing
and a `Term::Enum` to a `Declaration::External::Constant`.

`maybe-compile-time-value` of such a term answers its value (`pi` is
`3.141592653589793e0`, `True` is `Bool::True`) and `has-compile-time-value`
is the `Int` `1`; a term built by hand is not resolved, has no compile-time
value and its `maybe-compile-time-value` dies with
`This element has not been resolved.`. Part of #14916; the regression test is
`t/rakuast/rakuast-term-name-resolution.t`. `Term::Named` (`now`, `time`)
still has no resolution.
