use v6;
use Test;

# A `try` leaves its exception in `$!` whether or not a CATCH block handled it
# (a matching `when`/`default`, or `.resume`), as rakudo does (#12904).

plan 7;

# Handled by a matching `default` still sets $!.
try { die "PRIOR" };
is $!.message, 'PRIOR', 'unhandled exception is stored in $!';
try { die "H"; CATCH { default { } } };
is $!.message, 'H', 'a handled (default) exception is left in $!';

# An unhandled exception swallowed by a later try overwrites $!.
try { die "NEXT" };
is $!.message, 'NEXT', 'a later unhandled exception overwrites $!';

# `.resume` handles the exception: $! holds it.
$! = Nil;
try { die "SEED" };
is $!.message, 'SEED', 'seed $! with an unhandled exception';
try { fail "R"; CATCH { default { .resume } } };
is $!.message, 'R', 'a resumed exception is left in $!';

$! = Nil;
try { die "X"; CATCH { default { } } };
ok $!.defined, 'handled from a cleared $! leaves $! defined';

# Successful try resets $! (does not preserve a prior exception).
try { die "OLD" };
try { my $ok = 1 };
nok $!.defined, 'a successful try resets $!';
