use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# An exception that kills a sunk `start` block, with nothing left to observe
# its Promise, is fatal: Rakudo's scheduler reports it and exits with status
# 1, so the mainline does not run on past it (#9767). Measured against rakudo.

plan 6;

is_run 'start { die "dead" }; sleep 1; say "still alive"',
    { out => '', err => /'Unhandled exception in code scheduled on thread' .* 'dead'/, status => 1 },
    'a sunk start that dies ends the program';

is_run 'say "before"; start { sleep 0.2; die "dead" }; sleep 2; say "after"',
    { out => "before\n", err => /dead/, status => 1 },
    'the program ends when the worker dies, not when the mainline finishes';

is_run 'start { say "w"; die "dead" }; sleep 1; say "alive"',
    { out => "w\n", err => /dead/, status => 1 },
    'the worker\'s own output before it died is kept';

is_run 'my $p = start { die "dead" }; sleep 0.5; say $p.status',
    { out => "Broken\n", err => '', status => 0 },
    'a kept reference can still be observed: not fatal';

is_run 'start { die "dead" }.then({ say "then" }); sleep 1; say "alive"',
    { out => "then\nalive\n", err => '', status => 0 },
    'a .then subscriber observes the broken promise: not fatal';

is_run '$*SCHEDULER.uncaught_handler = -> $e { say "handled: ", $e.message };
        start { die "dead" }; sleep 1; say "alive"',
    { out => "handled: dead\nalive\n", err => '', status => 0 },
    'a user uncaught_handler replaces the default exit';
