use v6;
use Test;

plan 1;

# mutsu-specific guard (docs/security.md: recursion needs a bound that raises a
# catchable exception). A cyclic subset base made the type check recurse
# until the native stack overflowed and the process aborted (#15187,
# Physics::Unit). rakudo rejects the cycle at compile time; either way the
# program must not abort.
my $caught = False;
try {
    EVAL 'subset CycA of CycB; subset CycB of CycA; my CycA $x = 1; 1';
    CATCH { default { $caught = True } }
}
ok $caught, 'a cyclic subset base is an exception, not a stack overflow abort';
