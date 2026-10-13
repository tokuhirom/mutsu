use v6.d;
use Test;

# Test::Run (t/01-test-run.rakutest): `cmp-ok $got, &[R[~~]], $expected`.
# `R[op]` names the same operator as `Rop`; the brackets only group it.

plan 6;

is (&[R[~~]])("a", "a"), True, '&[R[~~]] called';
is (&[R[<=>]])(1, 2), More, '&[R[<=>]] swaps its operands';
is (&[R<=>])(1, 2), More, 'unbracketed spelling agrees';
is (&[Z[+]])((1, 2), (10, 20)).join(','), '11,22', '&[Z[+]]';
is (&[X[+]])((1, 2), (10, 20)).join(','), '11,21,12,22', '&[X[+]]';

my $op = &[R[~~]];
ok $op ~~ Callable, 'the reference is Callable';
