use Test;

# A named sub at the top level of an EVAL'd unit captures the unit's variables
# lexically, as a program's own mainline subs do: an assignment a nested EVAL
# makes by name to such a variable reaches the unit's own variable even when a
# caller of the sub has a variable of the same name.
plan 8;

use MONKEY-SEE-NO-EVAL;

is EVAL(Q[my $x = 1;
sub set-x() { EVAL q[$x = 'str']; $x }
sub int-caller() { my $x = 5; set-x() }
int-caller() ~ '/' ~ $x;]), 'str/str', 'a same-named caller variable does not take the store';

is EVAL(Q[my $x = 1;
sub set-x() { EVAL q[$x = 'str']; $x }
set-x() ~ '/' ~ $x;]), 'str/str', 'with no same-named caller variable';

is EVAL(Q[my $x = 1;
sub bump() { $x = $x + 1 }
sub other() { my $x = 100; bump(); $x }
bump(); other() ~ '/' ~ $x;]), '100/3', 'a plain store to the unit variable from a caller with its own';

is EVAL(Q[my @a = 1, 2;
sub add() { @a.push(3) }
sub caller-with-a() { my @a = 9; add(); @a.elems }
caller-with-a() ~ '/' ~ @a.elems;]), '1/3', 'an array of the unit';

# Two EVAL runs do not share cells.
my $r1 = EVAL(Q[my $y = 10; sub gy() { $y }; $y = 11; gy()]);
my $r2 = EVAL(Q[my $y = 20; sub gy() { $y }; gy()]);
is $r1, 11, 'the first unit reads its own variable';
is $r2, 20, 'the second unit reads its own variable';

# The program's own variables are untouched by the unit's.
my $x = 'outer';
EVAL(Q[my $x = 1; sub sx() { EVAL q[$x = 2]; $x }; sx()]);
is $x, 'outer', 'the unit variable is not the program variable';

# A sub of the unit is not callable once the unit is over.
EVAL(Q[sub only-in-unit() { 1 }]);
dies-ok { EVAL(Q[only-in-unit()]) }, 'a unit sub does not outlive its unit';
