# `s[PAT] OP= EXPR` is a substitution, with its operator, across RakuAST

`s[\d] += 5` and `s:g[\w] x= 2` used to be desugared by the parser into
`$_ = $_.subst(rx, { $/ OP EXPR }, :g)`. That had no RakuAST form (`.AST`
refused with "literal Regex without a declarator tree"), honoured only the
`:g` adverb, and answered the substituted Str where rakudo answers the Match
(a List of Matches under `:g`).

The parser now builds the same `Expr::Subst` as `s[PAT] = EXPR`, whose
per-match thunk is `$/ OP EXPR`, and records the written operator beside it
(`ast::SubstThunk::infix`). `.AST` renders it as rakudo 2026.09 does,
`Substitution(infix => MetaInfix::Assign(Infix(OP)), replacement => EXPR)`, and
lowering rebuilds the thunk through the same parser helper.

`t/rakuast/rakuast-subst-compound-assign.t` pins the tree, the EVAL direction
and the semantics. With it and the 27 files the 2026-10-13 survey found already
passing but unlisted, the round-trip ratchet grows by 30 files. Found on the
way: `EVAL q[$_ = "b"]` does not write through to the caller's `$_` (#13298).
