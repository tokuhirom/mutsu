# Heredoc and `qq` closure parts are Block call frames

A `{ … }` closure inside a heredoc, `qq{…}`, `qq:to` or an `s///` replacement
is now parsed into the same scope-isolated `DoStmt(Block(…))` the `"…"` parser
builds, so `callframe(0)` inside it is the `Block` and the enclosing routine is
one frame up — `qq:to` bodies used to report `Sub Nil` where Rakudo reports
`Block Sub` (#9999, follow-up to #9765).

This needed the assignment forms of substitution (`s[pat] = EXPR`,
`S[pat] = EXPR`) to stop pretending their RHS is a qq closure. The parser still
records the RHS source wrapped in `{…}`, but the `Subst` / `NonDestructiveSubst`
node and opcode now carry a `replacement_thunk` flag, and the run-time
replacement plan parses such a source as a bare thunk expression. A placeholder
in the RHS (`[3,4].map: { S{5} = $^a }`) therefore still belongs to the
enclosing block, while a placeholder in a real `s/…/{$^a}/` closure is rejected
as Rakudo rejects it.
