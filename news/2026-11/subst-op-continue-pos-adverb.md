# `s:c(N)///` and `s:p(N)///` start the substitution at N

A substitution's `:c` / `:continue` and `:p` / `:pos` adverbs reached only the
RakuAST source tree, so `s:c(2)/b/X/` on `abcabc` replaced the first `b`
instead of the second. `Expr::Subst` / `NonDestructiveSubst` now carry the
argument as an expression (`SubstStart`), `OpCode::Subst` pushes its value
like `:nth` / `:x`, and the substitution's scan starts there; `:p` anchors the
first match, as in Rakudo (#18892).
