use Test;

# `BUILDALL` and `POPULATE` are declared on `Mu`, so a user override that defers
# ends at them and gets the already-built instance back (ADR-11276 §9.47).

plan 6;

class A {
    has $.x = 1;
    method BUILDALL(|c) { my $r = callsame; $r }
}
my $a = A.new(x => 5);
is $a.x, 5, 'callsame from a BUILDALL override keeps the built attributes';
is $a.^name, 'A', '... and the instance';

class B {
    has $.y;
    method POPULATE(|c) { nextsame }
}
is B.new(y => 2).y, 2, 'nextsame from a POPULATE override keeps the built attributes';

class C {
    has $.z = 3;
    method BUILDALL(|c) { nextwith(|c) }
}
is C.new(z => 4).z, 4, 'nextwith from a BUILDALL override keeps the built attributes';

role R { method BUILDALL(|c) { callsame } }
class D does R { has $.w; }
is D.new(w => 6).w, 6, 'a role-provided BUILDALL defers the same way';

class E { has $.v = 7; method POPULATE(|c) { callsame } }
isa-ok E.new, E, 'a POPULATE override on a defaulted class still constructs';
