use v6;
use Test;

# A hyper-prefix operator (`+«`, `-«`, `~«`, and the ASCII `+<<` forms) is an
# unambiguous term start, so it may open a no-paren argument to a list operator:
# `unique +«(1,2,2,3)` parses as `unique(+«(1,2,2,3))`. Previously the general
# listop-argument gate only recognized sigils/parens/digits and rejected the
# leading `+`, so the whole expression failed to parse.

plan 7;

is-deeply unique(+«(1,2,2,3)), (1,2,3), 'paren form baseline';

# No-paren forms.
is-deeply (unique +«(1,2,2,3)), (1,2,3), 'unique +« (Unicode marker)';
is-deeply (unique +<<(1,2,2,3)), (1,2,3), 'unique +<< (ASCII marker)';
is-deeply (unique -«(1,-1,2)), (-1,1,-2), 'unique -« negates then dedups';
is-deeply (unique ~«(1,2,3)), ('1','2','3'), 'unique ~« stringifies';

# The narrow gate must not disturb a bare prefix/infix on a 0-ary term:
# `pi - 1` stays a subtraction, and `+<` stays the shift-left infix.
is (pi - 1).round(1e-6), 2.141593, 'pi - 1 is still subtraction';
is (3 +< 2), 12, '3 +< 2 is still shift-left';
