use Test;

# Track B slice 1: atomic-store element cells. `cas`/assignments on hash and
# array ELEMENTS in threaded context go through per-element ContainerRef
# cells instead of cloning the whole container per op (which made 30k cas ops
# on a 10k-entry hash cost ~300M entry copies — S17-lowlevel/thread.t test 28).
# These pin exactness (no lost update through the cells) and write-through
# coherence between cells and later plain access.

plan 5;

# Hash element cas churn: every increment lands, no key lost.
{
    my %seen;
    my $times = 500;
    %seen{^$times} = (0 xx $times);
    my @t = (1..3).map: { Thread.start({ cas %seen{$_}, {.succ} for ^$times; }) };
    .finish for @t;
    is ([+] %seen.values), 3 * $times, 'hash element cas: every increment lands';
    is +%seen.keys, $times, 'hash element cas: no key lost';
    is +%seen.values.grep({ $_ == 3 }), $times, 'hash element cas: each key hit exactly 3 times';
}

# Single hot key under heavy contention.
{
    my %h = x => 0;
    my @t = (1..4).map: { Thread.start({ cas %h<x>, {.succ} for ^500; }) };
    .finish for @t;
    is %h<x>, 2000, 'contended single hash element: exact count';
}

# Plain access after the cells exist stays coherent (read + assignment write
# through the cell, then a further cas sees the assigned value).
{
    my %h = x => 0;
    my @t = (1..2).map: { Thread.start({ cas %h<x>, {.succ} for ^200; }) };
    .finish for @t;
    %h<x> = %h<x> + 1000;
    my @t2 = (1..2).map: { Thread.start({ cas %h<x>, {.succ} for ^50; }) };
    .finish for @t2;
    is %h<x>, 1500, 'assignment and later cas write through the same element cell';
}
