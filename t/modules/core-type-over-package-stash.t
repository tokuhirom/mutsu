use Test;

# A bare type name inside package `P` means the CORE type when CORE has one,
# even when a `P::Name` package exists: CORE is found lexically before the
# package's stash is consulted. Only a name CORE does not know falls back to
# `P::Name` (#18386: Red's `method Supply(--> Supply)` inside
# `Red::ResultSeq`, which `use`s `Red::ResultSeq::Supply`).

plan 8;

class A::B { }
class A::Supply { }
class A::Thread { }
class A::Str { }
class A {
    method m(B $x)              { 'ok' }
    method k(--> Supply)        { Supplier.new.Supply }
    method t(--> Thread)        { Thread.new(code => { 1 }) }
    method s(--> Str)           { 'x' }
    method p(Supply $s --> Str) { $s.^name }
    method n                    { Supply.^name }
}

is A.m(A::B.new), 'ok', 'a name CORE lacks resolves to the package type';
is A.k.^name, 'Supply', '--> Supply is the CORE Supply';
is A.t.^name, 'Thread', '--> Thread is the CORE Thread';
is A.s, 'x', '--> Str is the CORE Str';
is A.p(Supplier.new.Supply), 'Supply', 'a Supply parameter accepts a CORE Supply';
is A.n, 'Supply', 'a bare Supply term is the CORE type';

module R::Supply { }
role R {
    method k(--> Supply) { supply { emit 1 } }
}
class C does R { }
is C.k.^name, 'Supply', '--> Supply in a role is the CORE Supply';

class D {
    class Supply { }
    method k(--> Supply) { Supply.new }
}
is D.k.^name, 'D::Supply', 'a type declared inside the package still hides CORE';
