# RakuAST: `$o.attr = value` crosses the `.AST` boundary

An assignment to a method call (`$foo.c = ()`, `$foo.d(1) = 2`) is lowered by the parser to the
internal `__mutsu_assign_method_lvalue` writeback call, which `.AST` rejected as a desugared
construct. It now renders as rakudo does: `ApplyInfix` with an `Assignment` infix over the
`ApplyPostfix`/`Call::Method` left side. A dynamically named method (`$o."$n"() = v`) stays the
boundary.

Found by working the `Code::Coverage` distribution; its `t/01-basic.rakutest` now gets past `.AST`
and stops at the missing `visit-children`/`origin` API, tracked in #11033.
