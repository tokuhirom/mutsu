use v6;
use Test;

# ADR-0077: every live frame's slots share one contiguous stack, and the
# executing frame is the window at the top of it. A GC root scan must therefore
# visit the WHOLE stack, not the window -- the frames below hold live values that
# nothing else references. Reading roots through the window instead was the one
# way this slice could break silently: the program keeps running and only frees
# values it should not have.
#
# Each test below holds a heap value in a slot of a frame that is then suspended
# by a deeper call, forces collector work while it is suspended, and checks the
# value on the way back out.

plan 4;

class Node { has $.v is rw; has $.next is rw }

# 1. A plain suspended-frame slot survives a deep call under GC pressure.
sub deep-scalar($n) {
    my $held = Node.new(v => $n);
    deep-scalar($n - 1) if $n > 0;
    return $held.defined && $held.v == $n;
}
ok deep-scalar(150), 'a suspended frame keeps its slot value across a deep call';

# 2. Cycle-capable objects (the collector's actual subject) held only by
#    suspended frames.
sub deep-cycle($n) {
    my $a = Node.new(v => $n);
    my $b = Node.new(v => $n * 2);
    $a.next = $b;
    $b.next = $a;             # a cycle: only the collector can free it
    my $sum = $n > 0 ?? deep-cycle($n - 1) !! 0;
    return $sum + $a.v + $a.next.v;
}
is deep-cycle(60), (^61).map({ $_ * 3 }).sum,
    'cyclic objects held only by suspended frames stay reachable';

# 3. Containers, not just scalars, in suspended frames.
sub deep-array($n) {
    my @held = $n, $n + 1, $n + 2;
    my %map = a => $n;
    deep-array($n - 1) if $n > 0;
    return @held.join(',') eq "$n,{$n + 1},{$n + 2}" && %map<a> == $n;
}
ok deep-array(120), 'suspended frames keep array and hash slots';

# 4. Recursion far deeper than the retired locals pool's 64-entry bound, which
#    is where a per-frame vector used to start allocating and freeing per call.
sub deep-count($n) {
    my $x = $n;
    return $x if $n == 0;
    return $x + deep-count($n - 1);
}
is deep-count(800), (1..800).sum, 'deep recursion past the old pool bound is correct';
