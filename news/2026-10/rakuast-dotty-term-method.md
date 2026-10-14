# RakuAST renders `.=` with a routine or a Callable as the method

A `.=` can call a term instead of a named method: `$x .= &f`, `$x .= $m`,
`$x .= "{…}"(42)`, and the declaration forms `my %opts .= &get-opts` and
`my $actions .= $module.actions.new` (from CSS::Stylesheet). `.AST` refused
all of them. They now render the way rakudo 2026.09 does: the method is a
`Call::TermAsMethod` over the term, or a `Call::QuotedMethod` for an
interpolated name. They also lower back to the parser's expansion.

The declaration's source form now keeps three things it used to drop: the
term, a postfix chain after the call (rakudo makes the declaration the
operand of the chain), and a shaped array's dimensions
(`my @c[2;2] .= new(…)`). `RakuAST::Initializer::CallAssign` gained its
`postfixish` accessor.

Six more `t/` files pass under `MUTSU_RAKUAST=1`. Two wrong answers found on
the way are filed separately: an inner `my $q .= meth` calls the method on
an outer `$q` (#13273), and `$ast.EVAL` leaks the unit's `my` variables into
the caller (#13274).
