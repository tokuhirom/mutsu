# A CHECK-phase rewrite of `$*CU` is recompiled

Rakudo compiles the `RakuAST::CompUnit` tree *after* the CHECK phase, so a closure queued on
`$*CU` (`add-check-code`, or pushed onto `$!check-phasers`) can rewrite the program: add an
argument to a call, swap a node. mutsu's program was compiled from the parser's tree before, so
such a rewrite (Qwiratry's `BodyRewrite` for `aggreduce`/`aggmap`) had no effect.

When check closures ran and the `$*CU` tree changed, `OpCode::RunCheckCode` (no mainline statement
has run yet) now lowers the rewritten tree and runs it in place of the compiled program.
Also: `RakuAST::Node.IMPL-REPLACE-CHILD`, and an imported type (`sub EXPORT` hands out `Sum`) is a
`Type::Simple` in the `$*CU` tree, whose source is parsed with the search paths of the unit's `use`
lines. Known gap (TODO in the opcode): the unit's BEGIN prologue and `use` lines run a second time
with the rewritten tree, and a lexical a check closure filled is not seen by the running program.
