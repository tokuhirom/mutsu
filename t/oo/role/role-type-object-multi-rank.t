use Test;

plan 4;

# A role type object argument (`R`, where `role R does P`) is nominally closer
# to a `P` parameter than to an untyped one: rakudo compares `P` against
# `Any` per parameter, whatever the argument. mutsu used to score both as
# unrelated to the role, so the candidates tied (PDF's PDF::COS::Coercer
# picked its `(Any:D $obj, $type)` fallback for `coerce-to(42, PDF::COS::Int)`).
role P { }
role R does P { }
role S does R { }

class A {
    multi method m($o, P $c) { 'P' }
    multi method m(Any:D $o, $t) { 'any' }
}
is A.m(42, R), 'P', 'a role that does P binds the P candidate';
is A.m(42, S), 'P', 'and so does a role two levels down';
is A.m(42, P), 'P', 'P itself';

multi sub s($o, P $c) { 'P' }
multi sub s(Any:D $o, $t) { 'any' }
is s(42, R), 'P', 'multi sub ranks the same way';
