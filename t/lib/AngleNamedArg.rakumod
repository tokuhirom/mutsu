unit module AngleNamedArg;

# A routine the caller's compiler cannot see statically, so a listop-style
# call site with named arguments compiles through the statement-call path
# (`CallArg::Named` + `MakeNamedArg` + `ExecCallPairs`) rather than through
# `Expr::Call`'s `CallFuncNamed`.
sub capture-of(|c) is export { c }

multi sub tolerant(Numeric $got, Numeric $expected, $desc = '') is export { 'plain' }
multi sub tolerant(
    Numeric $got, Numeric $expected, $desc = '', Numeric :$abs-tol is required
) is export { 'abs' }
multi sub tolerant(
    Numeric $got, Numeric $expected, $desc = '',
    Numeric :$abs-tol is required, Numeric :$rel-tol is required
) is export { 'both' }
