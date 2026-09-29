use v6;
use Test;

# Two closures made by ONE routine, each capturing that routine's `@`/`%`
# parameter bound to a DIFFERENT caller container, and each spawning a
# `start` block, must keep writing into their own caller's container. The
# spawn-time exclusion of parameter-bound aggregates from the name-keyed
# cross-thread store used to remember only the latest binding per name, so
# the older closure's `%g` was seeded into the store under `%g` and both
# closures' stores were merged through one copy (#10076).

plan 4;

{
    sub mk(%g) { -> $k { %g{$k} = 1; start { 1 } } }
    my %one;
    my %two;
    my &f = mk(%one);
    my &h = mk(%two);
    f('a'); h('b'); f('c'); h('d');
    await Promise.in(0.05);
    is %one.keys.sort.join(','), 'a,c', 'the first binding keeps its own stores';
    is %two.keys.sort.join(','), 'b,d', 'the second binding keeps its own stores';
}

{
    sub mk-arr(@g) { -> $v { @g.push($v); start { 1 } } }
    my @one;
    my @two;
    my &f = mk-arr(@one);
    my &h = mk-arr(@two);
    f(1); h(2); f(3); h(4);
    await Promise.in(0.05);
    is @one.join(','), '1,3', 'the first array binding keeps its own pushes';
    is @two.join(','), '2,4', 'the second array binding keeps its own pushes';
}
