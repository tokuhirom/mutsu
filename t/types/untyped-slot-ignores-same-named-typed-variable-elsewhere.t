use Test;

# A module declaring a typed `$s` / `$n` / `$i` must not change how the
# program's own UNTYPED variables of the same names behave, and a typed
# variable still enforces its constraint. The in-place `~=` append, `++` and
# the declaration lane consult the slot's own declaration (the compiler knows
# `my $s` has no constraint), not a per-name "some typed variable of this name
# exists somewhere" latch (#9170).
plan 10;

use lib 't/lib';
use TypedSameNameElsewhere;

is typed-s(), 3, 'the module ran its typed $s';
is typed-n(), 'a', 'and its typed $n';
is typed-i(), 2, 'and its typed $i';

my $s = '';
for ^1000 { $s ~= 'ab' }
is $s.chars, 2000, 'an untyped $s accumulates';

my $n = 0;
for ^1000 { $n++ }
is $n, 1000, 'an untyped $n counts';

my $i = 'x';
$i ~= 'y' for ^3;
is $i, 'xyyy', 'an untyped $i appends';

# The untyped variable takes any value afterwards: it has no constraint.
$s = 42;
is $s, 42, 'an untyped $s holds an Int';
$n = 'str';
is $n, 'str', 'an untyped $n holds a Str';

# A typed variable of the same name in this program still checks.
sub typed-here { my Int $t = 1; $t = 'no'; }
throws-like { typed-here() }, X::TypeCheck::Assignment, 'a typed store still checks its constraint';
sub typed-append { my Int $t = 1; $t ~= 'a'; }
throws-like { typed-append() }, X::TypeCheck::Assignment, 'a typed ~= still checks its constraint';
