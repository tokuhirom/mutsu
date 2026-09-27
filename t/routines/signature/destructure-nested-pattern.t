use v6;
use Test;

plan 13;

# A destructuring pattern nested inside another one -- positionally
# (`-> ($a, ($b, $c))`) or behind a named key (`-> (:value(($n)), |)`) -- must
# unpack its own element. Game::Entities iterates its views with
# `for E.view(Named) -> (:value(($name)), |) { ... }`.

# --- for-loop parameters -------------------------------------------------------

{
    my @r;
    for ((1, (2, 3)),) -> ($a, ($b, $c)) { @r.push: "$a $b $c" }
    is-deeply @r, ['1 2 3'], 'nested positional pattern in a for signature';
}

{
    my @r;
    for ((1, (2, 3)),) -> ($a, [$b, $c]) { @r.push: "$a $b $c" }
    is-deeply @r, ['1 2 3'], 'nested bracketed pattern in a for signature';
}

{
    my @r;
    for (1 => (10,),) -> (:value(($n)), |) { @r.push: $n }
    is-deeply @r, [10], 'a named key unpacked by a nested pattern';
}

{
    my @r;
    for (1 => (10, 20),) -> (:key($k), :value(($x, $y))) { @r.push: "$k $x $y" }
    is-deeply @r, ['1 10 20'], 'renamed key next to a destructured value';
}

{
    my @r;
    for (1 => (10, (20, 30)),) -> (:value(($x, ($y, $z))), |) { @r.push: "$x $y $z" }
    is-deeply @r, ['10 20 30'], 'three levels of nesting';
}

{
    my @r;
    for ((1, 2) => (3, 4),) -> (:key(($a, $b)), :value(($c, $d))) { @r.push: "$a $b $c $d" }
    is-deeply @r, ['1 2 3 4'], 'two named keys each unpacked by a nested pattern';
}

{
    my @l = (1, (2, 3)), (4, (5, 6));
    my @r;
    for @l -> ($a, ($b, $c)) { @r.push: "$a $b $c" }
    is-deeply @r, ['1 2 3', '4 5 6'], 'nested pattern over an array, every iteration';
}

# --- sub signatures --------------------------------------------------------------

{
    sub f((:value(($c, $d)), |)) { "$c $d" }
    is f(1 => (3, 4)), '3 4', 'a named key hands its whole value to the nested pattern';
}

{
    sub g((:key(($a, $b)), :value(($c, $d)))) { "$a $b $c $d" }
    is g((1, 2) => (3, 4)), '1 2 3 4',
        'two nested named patterns do not clash on a shared placeholder name';
}

{
    sub h((:value((:key($d), :value($e))), |)) { "$d $e" }
    is h(1 => (5 => 6)), '5 6', 'a named key unpacked as a Pair';
}

{
    sub k((:$key ($x, $y), |)) { "$key $x $y" }
    is k((7, 8) => 1), '7 8 7 8', 'a named variable with its own destructure keeps both';
}

{
    sub m((:value($v), |)) { $v }
    is m(1 => 2), 2, 'a plain renamed key is unchanged';
}

throws-like 'sub dup((:key(($a)), :value(($a)))) { }', X::Redeclaration,
    'a real duplicate inside nested named patterns is still a redeclaration';
