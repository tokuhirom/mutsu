use v6;
use Test;

# Regression: declaring a user class whose final name component collides with a
# core type (e.g. `Foo::Any`) and using it as a method parameter type registered
# an unqualified `Any` env binding. Type matching followed that alias, so
# `42 ~~ Any` — and untyped-parameter binding, which checks `Any` — wrongly
# resolved to the user `Foo::Any` and failed ("expected Any but got Str").
# Surfaced loading zef (Zef::Distribution::DependencySpecification::Any).

plan 8;

class Foo::Any {
    has @.specs;
}
class User {
    multi method m(Foo::Any $s) { 'any' }
    multi method m($s)          { 'other' }
}

# Core `Any` must still match ordinary values and reject nothing it shouldn't.
ok 42 ~~ Any,    '42 ~~ Any holds after a Foo::Any class+method param exists';
ok 'x' ~~ Any,   'Str ~~ Any holds';
ok Nil ~~ Any,   'Nil ~~ Any holds';
ok 3.14 ~~ Any,  'Rat ~~ Any holds';

# Untyped parameter binding (implicit Any) accepts a Str.
{
    sub takes-any($p) { "got:$p" }
    is takes-any('hello'), 'got:hello', 'untyped param binds a Str (implicit Any)';
}

# The user class still works as its own type.
{
    my $a = Foo::Any.new;
    ok $a ~~ Foo::Any, 'Foo::Any instance matches Foo::Any';
    is User.new.m($a), 'any', 'multi dispatch picks the Foo::Any candidate';
}

# A genuine user type alias (non-core name) is still followed.
{
    constant MyInt = Int;
    ok 7 ~~ MyInt, 'user type alias constant still resolves in type matching';
}
