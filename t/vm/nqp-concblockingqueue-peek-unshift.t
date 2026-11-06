use Test;
use nqp;

plan 6;

my class Q is repr('ConcBlockingQueue') { }

my $q := nqp::create(Q);
ok nqp::isnull(nqp::atpos($q, 0)), 'atpos of an empty queue is null';

nqp::push($q, "a");
nqp::push($q, "b");
is nqp::atpos($q, 0), "a", 'atpos 0 peeks the head';
dies-ok { nqp::atpos($q, 1) }, 'only the head can be peeked';
is nqp::elems($q), 2, 'peeking takes nothing';

nqp::unshift($q, "z");
is nqp::shift($q), "z", 'unshift puts an item at the head';
is nqp::shift($q), "a", 'the older items follow';

# vim: expandtab shiftwidth=4
