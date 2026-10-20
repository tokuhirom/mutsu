use v6;
use Test;

plan 5;

# A `my class A {...}` stub in a role body and its later definition are one
# class, also when the role body runs again for a composition: the stub then
# finds the class already defined and the definition completes that same class
# (#12907, PDF::COS::Tie's `COSAttr`).

role R {
    my class A {...}
    my role H { has A $.a is rw; }
    my class A { has $.v = 3 }
    method via-role { my $h = H.new; $h.a = A.new; $h.a.v }
    method typed { my A $x = A.new; $x.v }
    method ids { A.WHICH }
}

class C does R {}
class D does R {}

is C.new.via-role, 3, 'an attribute typed with the stub accepts the defined class';
is D.new.via-role, 3, 'same after a second composition';
is C.new.typed, 3, 'a variable typed with the stubbed name accepts an instance';
is C.new.ids, D.new.ids, 'both compositions see one class';

role S {
    my class B {...}
    my class B { method hi { 'hi' } }
    method go { B.hi }
}
class E does S {}
class F does S {}
is F.new.go, 'hi', 'a stubbed and defined class is usable after several compositions';
