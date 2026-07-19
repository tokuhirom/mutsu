use v6;
use Test;

# A Unicode numeric literal (vulgar fraction / superscript) as the FIRST
# argument of a no-paren listop call must parse as an argument, not strand
# the fraction as a separate statement.
# Regression: `atan2 ⅔, ⅓` used to leave `atan2` as a bare word.

plan 8;

is-approx atan2(⅔, ⅓), atan2(2/3, 1/3), 'atan2 ⅔, ⅓ (paren baseline)';

# no-paren listop, fraction first arg + comma
is-approx (atan2 ⅔, ⅓), atan2(2/3, 1/3), 'atan2 ⅔, ⅓ no-paren';
is-approx (atan2 ⅔, 1), atan2(2/3, 1), 'atan2 ⅔, 1 no-paren';
is-approx (atan2 ½, ¼), atan2(1/2, 1/4), 'atan2 ½, ¼ no-paren';

# single-arg named unary with a fraction still works
is-approx (sqrt ¼), 0.5, 'sqrt ¼ no-paren';
is-approx (cos ⅓), cos(1/3), 'cos ⅓ no-paren';

# plain-number path unaffected
is-approx (atan2 1, 2), atan2(1, 2), 'atan2 1, 2 no-paren (unchanged)';

# postfix superscript exponent unaffected by the new term-start rule
is 2², 4, 'superscript exponent 2² still parses';
