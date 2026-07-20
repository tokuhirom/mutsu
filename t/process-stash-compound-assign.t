use v6;
use Test;

# PROCESS::<$name> compound assignment (//=, ||=, +=) desugars its subscript
# into a runtime-key `PROCESS::{$temp}` assignment. That runtime-key write must
# reach the process-level dynamic store, not a throwaway pseudo-stash hash.

plan 9;

# //= on a fresh key installs the default and reads it back.
PROCESS::<$MUTSU_A> //= 5;
is PROCESS::<$MUTSU_A>, 5, '//= on fresh PROCESS key stores the value';
is $*MUTSU_A, 5, '//= write is visible via $*MUTSU_A dynamic read';

# //= on an already-defined key keeps the existing value.
PROCESS::<$MUTSU_B> = 3;
PROCESS::<$MUTSU_B> //= 99;
is PROCESS::<$MUTSU_B>, 3, '//= keeps the existing defined value';

# += compound assignment.
PROCESS::<$MUTSU_C> = 10;
PROCESS::<$MUTSU_C> += 5;
is PROCESS::<$MUTSU_C>, 15, '+= compound assign on PROCESS key';

# ||= replaces a falsy value.
PROCESS::<$MUTSU_D> = 0;
PROCESS::<$MUTSU_D> ||= 42;
is PROCESS::<$MUTSU_D>, 42, '||= replaces a falsy PROCESS value';

# The assignment is an expression and returns the (possibly-installed) value.
my $r = (PROCESS::<$MUTSU_E> //= 77);
is $r, 77, '//= returns the installed value as an expression';

# Runtime-key subscript spelling ({$k}) writes to the same place as <$name>.
my $k = '$MUTSU_F';
PROCESS::{$k} = 8;
is PROCESS::{$k}, 8, 'runtime-key {$k} write is readable via {$k}';
is PROCESS::<$MUTSU_F>, 8, 'runtime-key {$k} write is readable via <$name>';

# GLOBAL:: compound assignment still works (regression guard).
GLOBAL::<$MUTSU_G> //= 21;
is GLOBAL::<$MUTSU_G>, 21, '//= on GLOBAL key stores the value';
