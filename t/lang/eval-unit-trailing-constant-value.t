use Test;

# The value of an EVAL'd unit whose last statement is a `constant` declaration
# is the constant's value. The unit compiler reorders declarations ahead of
# other statements, which left a `SetLine` marker as the unit's last statement
# and the tail's value with it; only a one-statement unit with no marker kept
# its value.

plan 6;

is EVAL('constant TA = 5'), 5, 'a sigilless constant';
is EVAL('my constant TB = 6'), 6, 'a lexical constant';
is EVAL('constant @TC = 1, 2'), (1, 2), 'a constant list';
is EVAL("\nconstant TD = 7"), 7, 'a leading blank line';
is EVAL('constant TE = 8; 9'), 9, 'a later expression is the value';
is EVAL('my $v = 3'), 3, 'a variable declaration keeps its value';
