use Test;

# A named `sub` declared in a block (a loop body, a `.map` callback) closes
# over the block activation that declared it, as rakudo's `capturelex` does:
# each `&g` keeps its own iteration's lexicals (#12692, ADR-12529 phase 3).

plan 13;

{
    my @r = (1, 2).map(-> $v { my $q = $v; sub g { $q }; &g });
    is-deeply @r.map({ $_() }).List, (1, 2), '.map callback: each &g keeps its own iteration';
}

{
    my @s;
    for 1, 2 -> $v { my $q = $v; sub h { $q }; @s.push(&h) }
    is-deeply @s.map({ $_() }).List, (1, 2), 'for loop body: each &h keeps its own iteration';
}

{
    my @p;
    for 1, 2 -> $v { sub hp { $v }; @p.push(&hp) }
    is-deeply @p.map({ $_() }).List, (1, 2), 'a loop parameter read directly';
}

{
    sub mk($v) { my $q = $v; sub k { $q }; &k }
    my @t = mk(1), mk(2);
    is-deeply @t.map({ $_() }).List, (1, 2), 'routine body: each call keeps its own';
}

{
    my @w = (1, 2).map(-> $v { my $q = $v; sub inc { $q += 10 }; my &c = &inc; c(); $q });
    is-deeply @w.List, (11, 12), 'a write through the code object reaches the activation';
}

{
    my @l = (1, 2).map(-> $v { my $q = $v; sub g2 { $q }; my &c = &g2; $q = $v * 100; &c });
    is-deeply @l.map({ $_() }).List, (100, 200), 'a write after taking &g is seen by it';
}

{
    my @s = (1, 2).map(-> $v { my $q = $v; sub g4 { $q }; &g4 });
    sub caller-shadow(&f) { my $q = 'caller'; f() }
    is-deeply @s.map({ caller-shadow($_) }).List, (1, 2), "a caller's same-named lexical does not answer";
}

{
    my @k = (1, 2).map(-> $v { my $q = $v; sub g5 { (1, 2).map({ $q + $_ }).List }; &g5 });
    is-deeply @k.map({ $_() }).List, ((2, 3), (3, 4)), 'a closure made inside the sub sees the same activation';
}

{
    my @n = (1, 2).map(-> $v { my $q = $v; { my $q = $v * 7; sub g6 { $q }; &g6 } });
    is-deeply @n.map({ $_() }).List, (7, 14), 'the innermost declaration of the name';
}

{
    my @m;
    for 1, 2 -> $v { my @q = $v; sub g8 { @q.push(9); @q.join(',') }; @m.push(&g8) }
    is-deeply @m.map({ $_() }).List, ('1,9', '2,9'), 'an array free variable, mutated';
    is-deeply @m.map({ $_() }).List, ('1,9,9', '2,9,9'), '... and the mutation persists per activation';
}

{
    my @rw = (1, 2).map(-> $v { my $q = $v; sub r is rw { $q }; &r });
    @rw[0]() = 42;
    is-deeply @rw.map({ $_() }).List, (42, 2), 'an `is rw` sub returns its own activation\'s container';
}

{
    is-deeply (1, 2).map(-> $v { my $q = $v; sub g3 { $q * 2 }; g3() }).List, (2, 4),
        'a by-name call inside the block';
}
