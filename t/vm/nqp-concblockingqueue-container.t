use nqp;
use Test;

plan 4;

# `nqp::push`/`nqp::shift` find a ConcBlockingQueue however a bound operand
# arrives: directly, through a captured variable, through an `is raw`
# parameter (#17337).

my class Q is repr('ConcBlockingQueue') { }

my $q := nqp::create(Q);
nqp::push($q, 1);
is nqp::shift($q), 1, 'a bound queue';

my $c := nqp::create(Q);
my &put = -> $v { nqp::push($c, $v) };
put(3);
is nqp::shift($c), 3, 'a queue reached through a captured variable';

sub take($queue is raw) { nqp::shift($queue) }
nqp::push($q, 4);
is take($q), 4, 'a queue passed to an is raw parameter';

nqp::push($q, 'a');
nqp::unshift($q, 'b');
is nqp::shift($q), 'b', 'unshift puts the value at the head';
