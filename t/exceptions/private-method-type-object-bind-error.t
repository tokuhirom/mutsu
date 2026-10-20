use Test;

# A private method call on a type object whose arguments fail to bind
# reports the binding failure, not "No such private method", as the instance
# path already did.

plan 3;

class P {
    method !g(:$a!) { $a }
    method h { self!g(1) }
    method k { self!g(a => 3) }
    method !n(Int $x) { $x }
    method m { self!n("x") }
}

dies-ok { P.h }, 'a call whose arguments do not bind dies';
throws-like { P.m }, X::TypeCheck::Binding::Parameter, 'a type mismatch reports the binding failure';
is P.k, 3, 'a call that binds still runs';
