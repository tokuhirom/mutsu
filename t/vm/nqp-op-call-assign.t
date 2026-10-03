use Test;
use nqp;

# Assigning to an `nqp::` op call runs the op and writes through the container
# it returns, instead of looking the op up as an rw routine (#11451).

plan 5;

my int @i = 1, 2;
nqp::atposref_i(@i, 0) = 5;
is-deeply @i.List, (5, 2), 'nqp::atposref_i(...) = v stores into an int array';

my num @n = 1e0, 2e0;
nqp::atposref_n(@n, 1) = 5e0;
is-deeply @n.List, (1e0, 5e0), 'nqp::atposref_n(...) = v stores into a num array';

my str @s = 'a';
nqp::atposref_s(@s, 0) = 'z';
is-deeply @s.List, ('z',), 'nqp::atposref_s(...) = v stores into a str array';

my $l := nqp::list(1, 2);
throws-like { nqp::atpos($l, 0) = 5 }, X::Assignment::RO,
    'assigning to an op that returns a plain value dies as read-only';

my int @j = 7;
my $r := nqp::atposref_i(@j, 0);
$r = 8;
is @j[0], 8, 'the bound form keeps working';
