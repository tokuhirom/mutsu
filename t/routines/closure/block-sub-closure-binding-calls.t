use Test;

# A closure's call of a block-declared routine runs the routine of the
# activation the closure was created in, on the named-call path, and the
# inline .map path reads it through the closure's upvalue (#13049). Every
# expectation was checked against rakudo.

plan 8;
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h($x) { return $q + $x; 0 }; @a.push(-> $y { h($y) }) }
    is-deeply @a.map({ $_(10) }).List, (11, 12), 'return in the routine ends its call';
}
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h { &?ROUTINE.name ~ $q }; @a.push(-> { h() }) }
    is-deeply @a.map({ $_() }).List, ('h1', 'h2'), '&?ROUTINE is the routine';
}
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h { $q++ }; @a.push(-> { h(); h() }) }
    is-deeply @a.map({ $_() }).List, (2, 3), 'a write to the activation lexical sticks';
}
{
    my @a;
    for 3, 4 -> $v { my $q = $v; sub f($n) { $n <= 1 ?? $q !! $n * f($n - 1) }; @a.push(-> { f(3) }) }
    is-deeply @a.map({ $_() }).List, (18, 24), 'recursion through the routine';
}
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h(Int $x) { $q + $x }; my $s = 'x'; @a.push(-> { (try h($s)) // 'failed' }) }
    is-deeply @a.map({ $_() }).List, ('failed', 'failed'), 'a binding failure still throws';
}
{
    my @r;
    for 1, 2 -> $v { my $q = $v; sub h($x) { $q * $x }; @r.push((1, 2).map({ h($_) }).List) }
    is-deeply @r, [(1, 2), (2, 4)], 'map callback calls this activation\'s routine';
}
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h($x) { $q * $x }; @a.push(-> { (1, 2).map({ h($_) }) }) }
    is-deeply @a.map({ $_().List }).List, ((1, 2), (2, 4)), 'lazy map after the block exited';
}
{
    my @a;
    for 1, 2 -> $v { my $q = $v; sub h(:$n = 0) { $q + $n }; @a.push(-> { h(:n(10)) }) }
    is-deeply @a.map({ $_() }).List, (11, 12), 'named arguments';
}
