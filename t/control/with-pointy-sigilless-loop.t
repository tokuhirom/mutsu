use Test;

# `with X -> \d { ... }` inside a loop binds `d` to each iteration's value.
# The with's once-evaluated temp lives in a raw local slot (#18315); the
# sigilless parameter's declaration used to promote that temp to a shared
# container cell parked in the env, and the next iteration's read of the slot
# adopted the stale cell, so every iteration saw the first value (#18377,
# CSS::Properties kept only the first declaration of a style).

plan 7;

sub keys-of(@s) {
    my @d;
    for @s {
        with $_ -> \decl {
            my $prop := decl<i>;
            @d.push: $prop;
        }
    }
    @d
}
is-deeply keys-of([{:i<a>}, {:i<b>}, {:i<c>}]), [<a b c>],
    'with -> \d in a sub loop sees each iteration (hash elements)';

sub props(@style) {
    my @d;
    for @style {
        with .<property> -> \decl {
            @d.push: decl<ident>;
        }
    }
    @d
}
is-deeply props([{:property({:ident<color>})}, {:property({:ident<border>})}]),
    [<color border>], 'with .<k> -> \d in a loop sees each iteration';

sub arrays(@s) {
    my @d;
    for @s {
        with $_ -> \a { @d.push: a[0] }
    }
    @d
}
is-deeply arrays([[1], [2], [3]]), [1, 2, 3], 'with -> \d over arrays in a loop';

{
    my %h = :a(1);
    with %h -> \d { d<x> = 1 }
    is-deeply %h, {:a(1), :x(1)}, 'with %h -> \d still writes through to the hash';
}
{
    my @a = 1, 2;
    with @a -> \d { d.push: 3 }
    is-deeply @a, [1, 2, 3], 'with @a -> \d still pushes onto the array';
}
{
    my @in = {:i<a>}, {:i<b>};
    sub mark(@s) { for @s { with $_ -> \d { d<seen> = True } } }
    mark(@in);
    is-deeply @in.map(*<seen>).List, (True, True),
        'with -> \d in a loop writes into each element';
}
{
    my @d;
    for 1..3 {
        with Any { } orwith %(:v($_)) -> \h { @d.push: h<v> }
    }
    is-deeply @d, [1, 2, 3], 'orwith -> \d in a loop sees each iteration';
}
