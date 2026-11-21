# `fail(...)` is a `Call::Name` in RakuAST

`fail("nope")`, written with the parenthesis right after the name, is now
`.AST`'d as rakudo 2026.11 does: a `Call::Name` over the call's arguments,
with its origins, deparsing as `fail("nope")`. Before, the parser kept only
the failure value, and the view rebuilt it as the listop
`fail ("nope")` over a parenthesized argument. The statement still compiles
to the `fail` opcode, which records where the failure came from. The
arguments are now also kept as written, in a `ControlCallArgs` spelling.
`die(...)` already took the call path (#15780).
