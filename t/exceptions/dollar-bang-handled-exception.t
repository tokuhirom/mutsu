use v6;
use Test;

# `$!` is only *updated* to an exception when it propagates out of a `try`
# unhandled (swallowed by the implicit trap). When a CATCH block handles the
# exception — a matching `when`/`default`, or `.resume` — `$!` keeps whatever it
# held before the `try`. Regression: mutsu used to leave the handled exception in
# `$!` (doc-diff perl-var.rakudoc [2]).

plan 7;

# Handled by a matching `default` leaves the prior $! untouched.
try { die "PRIOR" };
is $!.message, 'PRIOR', 'unhandled exception is stored in $!';
try { die "H"; CATCH { default { } } };
is $!.message, 'PRIOR', 'a handled (default) exception keeps the prior $!';

# An unhandled exception swallowed by a later try overwrites $!.
try { die "NEXT" };
is $!.message, 'NEXT', 'a later unhandled exception overwrites $!';

# `.resume` handles the exception: prior $! is kept.
$! = Nil;
try { die "SEED" };
is $!.message, 'SEED', 'seed $! with an unhandled exception';
try { fail "R"; CATCH { default { .resume } } };
is $!.message, 'SEED', 'a resumed exception keeps the prior $!';

# Handling clears the *definedness* view: after a handled try starting from a
# cleared $!, $! is undefined again.
$! = Nil;
try { die "X"; CATCH { default { } } };
nok $!.defined, 'handled from a cleared $! leaves $! undefined';

# Successful try resets $! (does not preserve a prior exception).
try { die "OLD" };
try { my $ok = 1 };
nok $!.defined, 'a successful try resets $!';
