# This file deliberately does not `use Test`: loading Test enables reflective
# name access and sends the recursive sub through the env-mirroring path. The
# bug only appeared on the optimized positional-light path.

class Node { has $.v is rw; has $.next is rw }

sub deep-cycle($n) {
    my $a = Node.new(v => $n);
    my $b = Node.new(v => $n * 2);
    $a.next = $b;
    $b.next = $a;
    my $sum = $n > 0 ?? deep-cycle($n - 1) !! 0;
    return $sum + $a.v + $a.next.v;
}

my $got = deep-cycle(3);
say '1..1';
say $got == 18 ?? 'ok 1 - recursive cycles complete on positional-light' !! "not ok 1 - recursive cycles complete on positional-light (got $got)";
