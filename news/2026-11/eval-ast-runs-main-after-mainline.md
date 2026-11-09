# `EVAL($ast)` of a whole program dispatches its `MAIN`

Rakudo runs the `sub MAIN` of an EVAL'd RakuAST unit (`EVAL($program.AST)`)
after the unit's mainline, inside the unit; a string `EVAL` does not. The
lowered unit now ends with a `MAIN` dispatch when it declares one, so a
`sub MAIN(...);` file (whose body is the rest of the unit) runs as a program
under `EVAL($ast)`. `t/tooling/main-semicolon.t` joins the ratchet.
