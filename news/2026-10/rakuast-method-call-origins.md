# RakuAST method calls in a chain carry their origins

`.AST` now gives every method call of a chain the ranges Rakudo does. In
`@a.map({ $_ }).sum`, each partial chain (`@a.map({ $_ })`) has its own
`ApplyPostfix` origin, the `Call::Method` runs from the method name to the end
of the call, its `Name` covers the name, and its `ArgList` covers the inside of
the parentheses. A call on a curried `*` (`*.abs`) starts after the `*`, which
itself carries no origin, as on Rakudo.

The postfix loop notes each partial chain while it is built, so the
`ApplyPostfix` nodes get their origins through the usual exact-or-absent
attachment. The call's own parts are read off the source text after the
operand's range: a `.` (or `.?`, `.+`, `.*`, `.^`, `!`), the name, and an
optional parenthesized argument list. Anything else leaves them without an
origin rather than a guessed one.
