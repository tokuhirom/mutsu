use v6;
use Test;

# A parenthesized declaration used as an lvalue of a compound assignment
# yields the assigned container, and the whole form is ONE expression value:
# it must not corrupt the operands of an enclosing expression (the compile
# of `(my/state $x = init) op= rhs` leaked a stack slot, so
# `65 +< ((state $m = 24) -= 8)` shifted 16 by 16).

plan 6;

is 65 +< ((state $m = 24) -= 8), 4259840,
    'state decl compound assign inside a binary expression';
is 65 +< ((my $q = 24) -= 8), 4259840,
    'my decl compound assign inside a binary expression';
is ((my $x = 5) += 1), 6, 'paren my decl compound assign returns the result';
is $x, 6, 'the variable was updated';

my $c;
is 65 +< (($c = 24) -= 8), 4259840,
    'paren assignment lvalue compound assign inside a binary expression';

sub le-shift($v) { $v +< ((state $s = 24) -= 8) }
is-deeply (le-shift(1), le-shift(1), le-shift(1)), (65536, 256, 1),
    'state survives across calls with the right values';
