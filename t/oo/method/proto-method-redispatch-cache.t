use Test;

# The `{*}` redispatch of a `proto method` caches its winner per
# (class, method, argument types, governing owner) (#14868). Every case calls
# twice (the second call is the cached one) and must agree with the first.

plan 23;

class A { }
class B is A { }
class C { }

class P {
    proto method f(|) { {*} }
    multi method f(A:D $a) { 'A' }
    multi method f(B:D $a) { 'B' }
    multi method f(Mu:D $a) { 'mu' }
    multi method f(Int $n) { "int $n" }
    multi method f(Str $s) { "str $s" }
}

is P.f(A.new), 'A', 'A, first call';
is P.f(A.new), 'A', 'A, cached call';
is P.f(B.new), 'B', 'B beats A for a B';
is P.f(B.new), 'B', 'B, cached call';
is P.f(C.new), 'mu', 'C falls to Mu:D';
is P.f(C.new), 'mu', 'C, cached call';
is P.f(5), 'int 5', 'an Int';
is P.f(5), 'int 5', 'an Int, cached';
is P.f("x"), 'str x', 'a Str';
is P.new.f(7), 'int 7', 'an instance receiver';
is P.new.f(7), 'int 7', 'an instance receiver, cached';

# Definedness of the argument is part of the key.
class D {
    proto method g(|) { {*} }
    multi method g(Int:D $n) { 'defined' }
    multi method g(Int:U $n) { 'undefined' }
}
is D.g(1), 'defined', 'Int:D';
is D.g(Int), 'undefined', 'Int:U';
is D.g(2), 'defined', 'Int:D again';
is D.g(Int), 'undefined', 'Int:U again';

# Definedness of the invocant is part of the key.
class E {
    proto method h(|) { {*} }
    multi method h(E:D: $x) { 'instance' }
    multi method h(E:U: $x) { 'type object' }
}
is E.new.h(1), 'instance', 'instance invocant';
is E.h(1), 'type object', 'type-object invocant';
is E.new.h(1), 'instance', 'instance invocant again';

# A subclass of the proto's class resolves within the governing owner.
class Q is P { multi method f(A:D $a) { 'Q-A' } }
is Q.f(A.new), 'Q-A', 'a subclass candidate is seen through the parent proto';
is Q.f(A.new), 'Q-A', 'and cached';
is P.f(A.new), 'A', 'the parent still answers its own';

# No candidate: the error is raised on every call.
throws-like { P.f(1.5e0, 2) }, X::Multi::NoMatch, 'no matching candidate, first call';
throws-like { P.f(1.5e0, 2) }, X::Multi::NoMatch, 'no matching candidate, second call';
