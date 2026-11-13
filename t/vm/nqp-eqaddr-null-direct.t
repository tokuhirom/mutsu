use nqp;
use Test;

# `nqp::eqaddr` is object identity and `nqp::null` a no-op value, including
# as the body of an `nqp::while`.

plan 7;

my @a = 1, 2;
my @b = 1, 2;
ok nqp::eqaddr(@a, @a), 'an array is itself';
nok nqp::eqaddr(@a, @b), 'an equal array is another object';
ok nqp::eqaddr(Order::Less, Less), 'an enum value is itself';
nok nqp::eqaddr(Less, More), 'enum members differ';
ok nqp::eqaddr((1 cmp 2), Order::Less), 'a computed Order';

my int $i = 0;
nqp::while(nqp::islt_i(($i = nqp::add_i($i, 1)), 10), nqp::null);
is $i, 10, 'nqp::null as a while body';
ok nqp::isnull(nqp::null), 'nqp::null is null';
