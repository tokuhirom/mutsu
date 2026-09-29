# The `s[pat] = EXPR` RHS is a compiled thunk

The assignment forms of substitution, `s[pat] = EXPR` and `S[pat] = EXPR`, used
to keep `EXPR` as source text: the parser wrapped it in `{…}` and every
execution re-parsed that text at run time and evaluated it once per match
through the tree-walking carrier. The re-parse ran in a fresh scope, so a bare
anonymous `state` (`$++`, `++$`) in `EXPR` was declared *inside* the per-match
body and restarted on every match: `$_ = "aaa"; s:g{a} = $++` gave `000`
where Rakudo gives `012`.

`EXPR` is now carried as an AST on `Expr::Subst` / `Expr::NonDestructiveSubst`
and compiled once, at compile time, as a closure that the substitution op
calls per match (#10131). The closure takes the match as its own `$/`, so the
thunk sees each match's captures and never the match that ran before the
substitution. Everything else in it resolves in the enclosing scope, where the
parser declared it: the anonymous `state` counts across matches and across
calls of the enclosing routine, and a placeholder (`$^a`) stays the enclosing
block's. The run-time thunk re-parser (`parse_subst_thunk_replacement`) is
gone, and the replacement-plan cache is back to one entry per `qq` source.

Along the way `$0`, `$1`, ... compile to `$/[0]`, `$/[1]`, ... in a block that
declares its own `$/` (a `-> $/ { }` or `method m($/)` parameter), so
`-> $/ { $0 }` reads its parameter instead of whichever match ran last.
