# Subset parameter binding failures read like rakudo's (`UInt` included)

`taurus`'s `t/01-seconds-to-str.rakutest` checks that passing `-1` to
`sub seconds-to-str(UInt $seconds --> Str)` dies with a message matching
`/:s Constraint type check failed in binding to parameter/`. mutsu said
`Type check failed in binding to parameter ...` instead: its "a subset failure
is a constraint failure" branch only consulted user-declared subsets, and core's
`UInt` (`subset UInt of Int where * >= 0`) is not one.

Checking that against rakudo turned up two more differences in the same
branch, now fixed together:

- The constraint wording applies only when the value already is of the
  subset's refinee type and the `where` rejected it. `subset P of Int` given
  `"a"` is a plain `Type check failed ...; expected P but got Str ("a")`
  (mutsu said `Constraint ...` and printed the string unquoted).
- The expected type is named without its smiley: `P:D` and `UInt:D` read
  `expected P` / `expected UInt`.

Pinned by `t/oo/subset-param-binding-failure-message.t`. With the #7988 parse
fix for hyper hash subscripts, every `taurus` test file now matches rakudo.
