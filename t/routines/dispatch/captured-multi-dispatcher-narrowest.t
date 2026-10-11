use Test;

# A multi dispatcher captured out of the scope of its (non-`our`) proto
# ranks its captured candidates by narrowness, as a by-name call would --
# not in declaration order (#12474).

plan 10;

module M1 {
    proto p1(|) {*}
    multi sub p1(@a, @b) { 1 }
    multi sub p1(Array $a, Array $b) { 2 }
    multi sub p1(Int $x where * > 10) { "big" }
    multi sub p1(Int $x) { "int" }
    multi sub p1(Cool $x) { "cool" }
    multi sub p1($x, $y, $z) { "any3" }
    multi sub p1(Int $x, $y, $z) { "int3" }
    our sub g() { &p1 }
}

my &f = M1::g();
is f([1], [2]), 2, 'Array candidate beats the earlier @ candidate';
is f((1,), (2,)), 1, 'a List falls back to the @ candidate';
is f(5), 'int', 'Int beats the later Cool';
is f(50), 'big', 'a where refinement beats the plain Int';
is f('s'), 'cool', 'a Str reaches the Cool candidate';
is f(1, 2, 3), 'int3', 'a typed first param beats the earlier untyped one';
is f('a', 2, 3), 'any3', 'the untyped candidate still takes the rest';

module M2 {
    proto q(|) {*}
    multi sub q(Int $x, $y) { "a" }
    multi sub q($x, Int $y) { "b" }
    multi sub q(Str $x) { "s" }
    our sub g() { &q }
}

my &h = M2::g();
throws-like { h(1, 2) }, X::Multi::Ambiguous, 'incomparable candidates are ambiguous';
throws-like { h(1.5) }, X::Multi::NoMatch, 'no applicable candidate is NoMatch';
is h('x'), 's', 'a single applicable candidate is called';
