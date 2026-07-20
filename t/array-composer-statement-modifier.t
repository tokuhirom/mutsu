use v6;
use Test;

plan 10;

# An array composer accepts an inline statement modifier on its element(s):
# `[ EXPR for LIST ]`, `[ EXPR if COND ]`, and `;`-sectioned forms where each
# section carries its own modifier (`[ 'a' if $x; |@b if $y ]`). mutsu previously
# parsed the element and read the trailing `for`/`if` as a second term
# ("Two terms in a row" / "couldn't find final ']'"). Seen in the DataStar dist.

is-deeply [ 5 for 1, 2 ],       [5, 5],       '[ EXPR for LIST ]';
is-deeply [ $_ * 2 for 1..3 ],  [2, 4, 6],    '[ EXPR(topic) for LIST ]';
is-deeply [ 5 if 1 ],           [5],          '[ EXPR if TRUE ]';
is-deeply [ 5 if 0 ],           [],           '[ EXPR if FALSE ] is empty';

# Semicolon-sectioned composer where sections carry modifiers.
my $x = True;
my $y = False;
is-deeply [ 1 if $x; 2 if $x ], [1, 2],       'two sections, both modifiers true';
is-deeply [ 1 if $x; 2 if $y ], [1],          'second section modifier false drops it';
is-deeply [ 1 if $y; 2 if $x ], [2],          'first section modifier false drops it';

# Slip with a modifier in a section (the DataStar pattern).
my @b = 3, 4;
is-deeply [ 'a' if $x; |@b if $x ], ['a', 3, 4], 'slip section with a modifier';

# Plain composers are unchanged.
is-deeply [1, 2, 3],  [1, 2, 3], 'plain comma list still works';
is-deeply [1, 2; 3, 4], [(1, 2), (3, 4)], 'plain semicolon sections still work';
