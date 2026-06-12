use v6;
use Test;

plan 14;

# Phase 3 cell-CAS: cas/atomic ops on instance attributes operate directly on
# the instance's shared attribute cell (its write lock is the atomic
# primitive), with no shared_vars side channel.

class Box {
    has $!x = 10;
    has atomicint $.n = 0;

    method cas-swap() { cas($!x, 10, 20) }
    method cas-miss() { cas($!x, 999, 30) }
    method cas-block() { cas($!x, -> $v { $v + 5 }) }
    method bump() { $!n⚛++ }
    method drop() { $!n⚛-- }
    method fetch-n() { atomic-fetch($!n) }
    method x() { $!x }
}

my $b = Box.new;
is $b.cas-swap, 10, '3-arg cas returns the old value on swap';
is $b.x, 20, '3-arg cas stores the new value when expected matches';
is $b.cas-miss, 20, '3-arg cas returns the current value on mismatch';
is $b.x, 20, '3-arg cas leaves the attribute unchanged on mismatch';
is $b.cas-block, 25, '2-arg cas returns the block result';
is $b.x, 25, '2-arg cas stores the block result';

$b.bump; $b.bump; $b.bump; $b.drop;
is $b.n, 2, 'atomic increment/decrement on attribute accumulates';
is $b.fetch-n, 2, 'atomic-fetch reads the attribute';

# Cross-instance isolation: each instance has its own cell.
class Counter {
    has atomicint $!c = 0;
    method add() { cas($!c, -> $v { $v + 1 }) }
    method c() { $!c }
}
my $c1 = Counter.new;
my $c2 = Counter.new;
$c1.add; $c1.add; $c2.add;
is $c1.c, 2, 'cas on instance 1 stays in its own cell';
is $c2.c, 1, 'cas on instance 2 stays in its own cell';

# Atomic increments from a closure (cross-frame cell visibility).
my $bump = -> { $b.bump };
$bump(); $bump();
is $b.n, 4, 'atomic attr increments through a closure accumulate';

# Cross-thread atomic increment: no lost updates (the lost-update race was the
# per-method-exit whole-map cell commit clobbering concurrent cell RMWs).
class Tally {
    has atomicint $.count = 0;
    method incr() { $!count⚛++ }
}
my $t = Tally.new;
my @p = (1..4).map: { start { $t.incr for ^250 } };
await @p;
is $t.count, 1000, 'cross-thread atomic attr increments lose no updates';

# Cross-thread 2-arg cas accumulation.
class Sum {
    has $!total = 0;
    method add($v) { cas($!total, -> $cur { $cur + $v }) }
    method total() { $!total }
}
my $s = Sum.new;
my @q = (1..4).map: -> $i { start { $s.add($i) for ^100 } };
await @q;
is $s.total, 100 * (1 + 2 + 3 + 4), 'cross-thread 2-arg cas loses no updates';

# Parent sees the swapped value immediately after await (shared cell, no
# post-await collection step).
class Flag {
    has $!state = 'init';
    method set() { cas($!state, 'init', 'done') }
    method state() { $!state }
}
my $f = Flag.new;
await start { $f.set };
is $f.state, 'done', 'parent observes a child-thread cas through the shared cell';
