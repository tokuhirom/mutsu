use Test;

plan 8;

# A missing required named parameter throws X::AdHoc at runtime (NOT the
# compile-time arity X::TypeCheck::Argument that a missing positional yields).

# Simple all-named sub (VM light-call path).
{
    my $ex;
    sub f(:$x!) { }
    try { f(); CATCH { default { $ex = $_ } } };
    ok $ex.defined, 'missing required named param dies';
    isa-ok $ex, X::AdHoc, 'it is an X::AdHoc';
    is $ex.message, q{Required named parameter 'x' not passed}, 'message is correct';
}

# Typed named param (heavy bind path).
{
    my $ex;
    sub g(Int :$x!) { }
    try { g(); CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::AdHoc, 'typed required named param -> X::AdHoc';
}

# One of several required named params missing.
{
    my $ex;
    sub h(:$x!, :$y!) { }
    try { h(:x(1)); CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::AdHoc, 'second missing required named -> X::AdHoc';
    is $ex.message, q{Required named parameter 'y' not passed}, 'names the missing param';
}

# A missing required *positional* is still X::TypeCheck::Argument (regression guard).
{
    my $ex;
    sub p($x) { }
    try { p(); CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::TypeCheck::Argument, 'missing positional -> X::TypeCheck::Argument';
    is $ex.signature, '($x)', 'positional error carries .signature';
}
