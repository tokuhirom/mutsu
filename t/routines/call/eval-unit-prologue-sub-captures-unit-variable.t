use Test;

# An EVAL'd unit with a BEGIN-time effect (a BEGIN, a constant, a use) splits
# a declaration into a static half and a run-time half. Its named subs still
# capture the unit's variables lexically: a store a nested EVAL makes by name,
# and a later `:=` binding, reach the unit's own variable even when a caller of
# the sub has a variable of the same name.
plan 6;

use MONKEY-SEE-NO-EVAL;

is EVAL(Q[my $x = 1;
sub set-x() { EVAL q[$x = 'str']; $x }
sub c() { my $x = 5; set-x() }
BEGIN { 1 };
c(); $x]), 'str', 'a nested EVAL store reaches the unit variable (BEGIN prologue)';

is EVAL(Q[my $src = 10;
my $alias := $src;
sub bump-alias { $alias++ }
BEGIN { 1 };
bump-alias(); $src]), 11, 'a sub writes through a later container binding';

is EVAL(Q[my $x = 1;
sub bump() { $x = $x + 1 }
sub other() { my $x = 100; bump(); $x }
constant K = 3;
bump(); other() ~ '/' ~ $x ~ '/' ~ K;]), '100/3/3', 'a constant prologue, caller with its own variable';

is EVAL(Q[my @a = 1, 2;
sub add() { @a.push(3) }
sub caller-with-a() { my @a = 9; add(); @a.elems }
BEGIN { 1 };
caller-with-a() ~ '/' ~ @a.elems;]), '1/3', 'an array of the unit';

# Two EVAL runs do not share cells.
my $r1 = EVAL(Q[my $y = 10; sub gy() { $y }; BEGIN { 1 }; $y = 11; gy()]);
my $r2 = EVAL(Q[my $y = 20; sub gy() { $y }; BEGIN { 1 }; gy()]);
is $r1 ~ '/' ~ $r2, '11/20', 'two runs of a unit with a prologue keep their own variables';

my $x = 'outer';
EVAL(Q[my $x = 1; sub sx() { EVAL q[$x = 2]; $x }; BEGIN { 1 }; sx()]);
is $x, 'outer', 'the unit variable is not the program variable';
