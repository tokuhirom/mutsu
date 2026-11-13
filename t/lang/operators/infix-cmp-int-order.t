use nqp;
use Test;

# `cmp` of two Ints is their integer order, and the Order it answers is the
# same enum value as `Less` / `Same` / `More` for identity tests.

plan 10;

is 1 cmp 2, Less, '1 cmp 2';
is 2 cmp 2, Same, '2 cmp 2';
is 3 cmp 2, More, '3 cmp 2';
is -5 cmp 3, Less, 'negative against positive';
is 9223372036854775807 cmp -9223372036854775808, More, 'the extremes';
ok (1 cmp 2) === Order::Less, '=== Order::Less';
ok nqp::eqaddr((2 cmp 2), Order::Same), 'nqp::eqaddr against Order::Same';
nok nqp::eqaddr((2 cmp 3), Order::Same), 'nqp::eqaddr tells members apart';
my &c = &infix:<cmp>;
is c(7, 7), Same, 'through &infix:<cmp>';
is (10 cmp 9.5), More, 'an Int against a Rat still compares numerically';
