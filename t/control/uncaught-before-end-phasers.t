use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 2;

# #11020: rakudo's top-level handler reports an uncaught mainline exception
# first and only then runs the END phasers.

is_run ｢END note "e"; die "y"｣,
    %(:out(''), :err({ .index('y').defined && .index('e').defined && .index('y') < .rindex("e\n") }), :status(1)),
    'the uncaught exception is printed before END phaser output';

is_run ｢END note "e"; say "ok"｣,
    %(:out("ok\n"), :err("e\n"), :status(0)),
    'a normal exit still runs END phasers';
