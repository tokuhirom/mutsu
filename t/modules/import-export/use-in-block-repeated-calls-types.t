use Test;

# A `use` inside a routine body whose module exports types (a class, a
# subset, an enum) replays its first run's effects on later calls, as one
# that exports only routines does (#13906): the type names, the enum values
# and the package-type aliases they add. Every call must still see them,
# none of them may outlive the routine, and an outer import of the same
# names comes back after each call.
plan 9;

use lib 't/lib';

sub thing-of($n) { use UseInBlockRerunTypes; UibrtThing.new(:$n).twice }
is (1..4).map({ thing-of($_) }).join(','), '2,4,6,8', 'every call reaches the imported class';

sub small-of($n) { use UseInBlockRerunTypes; $n ~~ UibrtSmall }
is (5, 50, 7).map({ small-of($_) }).join(','), 'True,False,True', 'every call checks against the imported subset';

sub color-of($i) { use UseInBlockRerunTypes; UibrtColor($i).key }
is (0, 1, 0).map({ color-of($_) }).join(','), 'UibrtRed,UibrtGreen,UibrtRed', 'every call reaches the imported enum';

sub typed-of($n) { use UseInBlockRerunTypes; my UibrtThing $t = uibrt-make($n); $t.n }
is (1..3).map({ typed-of($_) }).join(','), '1,2,3', 'every call types a variable with the imported class';

sub value-of() { use UseInBlockRerunTypes; UibrtGreen.value }
is (^3).map({ value-of() }).join(','), '1,1,1', 'every call reaches an imported enum value';

dies-ok { EVAL 'UibrtThing.new' }, 'the class is not visible after the calls';
dies-ok { EVAL 'UibrtRed' }, 'nor is the enum value';

{
    use UseInBlockRerunTypes;
    sub inner-of($n) { use UseInBlockRerunTypes; UibrtThing.new(:$n).n }
    inner-of($_) for ^3;
    is UibrtThing.new(:n(5)).n, 5, 'an enclosing import of the same names survives the calls';
    ok 3 ~~ UibrtSmall, '... and so does its subset';
}

# vim: expandtab shiftwidth=4
