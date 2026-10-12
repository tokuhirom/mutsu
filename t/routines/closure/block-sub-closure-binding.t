use Test;

# ADR-12529 phase 2 (#12850, #12851): a `sub` declared in a block, loop body
# or closure body is that block's lexical `&name` binding for the closures
# nested in it. A closure keeps the routine of the activation it was created
# in -- after the block exited, and while a later activation runs. Every
# expectation was checked against rakudo.

plan 16;

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h { $q }; @a.push(-> { h() }) }
    is-deeply @a.map({ $_() }).List, (1, 2),
        'a closure calls its loop iteration\'s routine after the loop exited (#12850)';
}

{
    my @a = (1, 2).map(-> $v { my $q = $v; sub h { $q }; -> { h() } });
    is-deeply @a.map({ $_() }).List, (1, 2),
        'the same from a .map callback';
}

{
    my @r = (1, 2).map(-> $v { my $q = $v; sub g { $q }; my $c = { &g }; $c() });
    is-deeply @r.map({ $_() }).List, (1, 2),
        '`&g` taken in a closure nested in the declaring block is that activation\'s (#12851)';
}

{
    my $c;
    my @seen;
    for 1, 2 -> $v { my $q = $v; sub h { $q }; @seen.push($c()) if $c; $c = -> { h() } }
    is-deeply @seen, [1], 'a closure from an earlier iteration calls its own routine while a later one runs';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub k { $q }; @a.push(-> { -> { k() } }) }
    is-deeply @a.map({ $_()() }).List, (1, 2), 'a closure nested two deep reaches the binding';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub k { $q }; @a.push(-> { (1).map({ -> { k() } })[0] }) }
    is-deeply @a.map({ $_()() }).List, (1, 2), 'through a .map callback that creates the closure';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub k { $q }; @a.push(-> { (1).map({ k() }) }) }
    is-deeply @a.map({ $_().List }).List, ((1,), (2,)), 'a lazy .map run after the creating frame returned';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub m($x, :$n = 0) { $q * 10 + $x + $n }; @a.push(-> { m(1, :n(100)) }) }
    is-deeply @a.map({ $_() }).List, (111, 121), 'positional and named arguments reach the routine';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub m($x) { $q + $x }; @a.push(-> { &m(10) }) }
    is-deeply @a.map({ $_() }).List, (11, 12), 'an explicit `&m(...)` call';
}

{
    my @a;
    my $i = 0;
    while $i < 2 { $i++; my $q = $i; sub w { $q }; @a.push(-> { w() }) }
    is-deeply @a.map({ $_() }).List, (1, 2), 'a `while` body';
}

{
    my @b;
    { my $q = 'bare'; sub bb { $q }; @b.push({ bb() }) }
    is @b[0](), 'bare', 'a bare block';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; @a.push(-> { early() }); sub early { "early$q" } }
    is-deeply @a.map({ $_() }).List, <early1 early2>, 'a closure created before the declaration';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub s { $q }; @a.push(-> { my &s = -> { 'shadow' }; s() }) }
    is-deeply @a.map({ $_() }).List, <shadow shadow>, 'a `my &name` in the closure shadows the block\'s routine';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub r { return $q; 'never' }; @a.push(-> { r() ~ '!' }) }
    is-deeply @a.map({ $_() }).List, <1! 2!>, '`return` in the routine returns from it';
}

{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub wq { $q++ }; @a.push(-> { wq(); wq() }) }
    is-deeply @a.map({ $_() }).List, (2, 3), 'the routine writes its own activation\'s lexical';
}

{
    my &c = do { my sub bar() { 'closure outer' }; -> { bar() } };
    sub bar() { 'global bar' }
    is c(), 'closure outer', 'a closure keeps calling the `my sub` of its scope over a later same-named one';
}
