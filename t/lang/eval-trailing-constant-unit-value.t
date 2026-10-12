use Test;

plan 6;

# A trailing `constant` is hoisted to BEGIN time, but it is still the
# statement whose value the EVAL'd unit has.
is EVAL('42; constant TC = 5'), 5, 'constant after a declaration';
is EVAL('constant TA = 5'), 5, 'a unit of one constant';
is EVAL('constant \sl = 9'), 9, 'sigilless constant';
is-deeply EVAL('constant @ca = 1, 2'), (1, 2), 'array constant';
is EVAL('my $x = 3; constant TX = 6; $x + 1'), 4, 'a later expression still wins';
is EVAL('my $z = 7'), 7, 'a trailing plain declaration is unchanged';
