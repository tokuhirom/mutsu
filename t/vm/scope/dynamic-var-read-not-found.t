use Test;

plan 9;

# #9771: reading a genuinely undeclared dynamic variable used to read back as
# a plain `Nil`. Real raku raises `X::Dynamic::NotFound` -- as a lazy Failure,
# not an eager throw, so `.^name`/`.defined` still answer and only a context
# that actually sinks the value (`say`, string interpolation, ...) explodes it.

# A sub reading a caller's `$*x` sees only a plain (non-dynamic) lexical `$x`
# in the enclosing block -- `$*x` was never declared anywhere in the dynamic
# scope, so `say $*x` explodes the Failure it reads back.
sub reads-undeclared-dynvar() { say $*x }
{
    my $x = 41;
    my $err;
    try { reads-undeclared-dynvar(); CATCH { default { $err = $_ } } }
    ok $err.defined, 'say on an undeclared $*var read explodes';
    is $err.^name, 'X::Dynamic::NotFound', 'correct exception type';
    is $err.message, 'Dynamic variable $*x not found', 'correct message';
}

# `.^name` is a meta-method call: it must answer even on an unhandled Failure.
is $*nope.^name, 'Failure', '.^name on an undeclared $*var does not throw';

# `.defined` must answer False without exploding the Failure.
nok $*nope.defined, '.defined on an undeclared $*var is False, not a throw';

# @* and %* forms behave the same as $*.
is @*nope-arr.^name, 'Failure', '.^name on an undeclared @*var does not throw';
nok @*nope-arr.defined, '.defined on an undeclared @*var is False';
is %*nope-hash.^name, 'Failure', '.^name on an undeclared %*var does not throw';
nok %*nope-hash.defined, '.defined on an undeclared %*var is False';

# vim: expandtab shiftwidth=4
