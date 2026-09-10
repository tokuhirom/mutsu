use v6;
use Test;

# The MOP pseudo-methods WHAT/HOW/VAR are also first-class `&`-callables in
# Raku: `&WHAT`, `.map(&WHAT)`, `my $f = &WHAT; $f($x)`. Previously mutsu only
# handled the call-syntax forms (`WHAT($x)`), so `&WHAT` resolved to Any and
# `.map(&WHAT)` threw "Cannot map a Any to a Seq".

plan 8;

# `&WHAT` is a usable callable value.
{
    my $f = &WHAT;
    is $f(42).gist, '(Int)', 'my $f = &WHAT; $f(Int) works';
    is $f("x").gist, '(Str)', 'the same callable applied to a Str';
    is $f(3.14).gist, '(Rat)', 'the same callable applied to a Rat';
}

# `.map(&WHAT)` over a plain list.
is (1, 2, 3).map(&WHAT).gist, '((Int) (Int) (Int))', '.map(&WHAT) over a List';

# `.map(&WHAT)` over set/bag keys (the doc-example shape).
is (set <a b c>).keys.map(&WHAT).sort.gist, '((Str) (Str) (Str))',
    '.map(&WHAT) over Set keys';
is (bag "a" => 0, "b" => 1).keys.map(&WHAT).gist, '((Pair) (Pair))',
    '.map(&WHAT) over Bag keys built from Pairs';

# `&HOW` and `&VAR` are likewise first-class.
ok (&HOW.defined), '&HOW resolves to a defined callable';
is (10, 20).map(&VAR).gist, '(10 20)', '.map(&VAR) returns the containers';
