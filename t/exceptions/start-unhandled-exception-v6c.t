use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# `use v6.c`: a sunk `start` whose exception nobody looked at is NOT fatal
# (the fatal "Unhandled exception in code scheduled" report is 6.d+), so the
# mainline carries on and the program exits normally.

plan 3;

is_run 'use v6.c; start { die "dead" }; sleep 0.5; say "still alive"',
    { out => "still alive\n", err => '', status => 0 },
    'v6.c: a sunk start that dies does not end the program';

is_run 'use v6.c; my $p = start { die "dead" }; sleep 0.3; say $p.status',
    { out => "Broken\n", err => '', status => 0 },
    'v6.c: the Promise is still Broken';

is_run 'use v6.d; start { die "dead" }; sleep 0.5; say "still alive"',
    { out => '', err => /'Unhandled exception in code scheduled on thread' .* 'dead'/, status => 1 },
    'v6.d: the same program is fatal';
