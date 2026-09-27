use Test;

# `Signature ~~ Signature` compares parameter types with the matcher's type
# accepting the topic's (Rakudo's `Parameter.ACCEPTS` is
# `$!type.ACCEPTS(other.type)`), and that holds for user classes and roles,
# not only for the built-in type table. Found through Tinky, whose
# `subset ValidateCallback of Callable where { .signature ~~ :(Object --> Bool) }`
# must accept `sub (ObjectOne $) returns Bool { ... }` for a class doing `Object`.

plan 9;

role Object { }
class ObjectOne does Object { }
class Base { }
class Derived is Base { }

ok  sub (ObjectOne $) returns Bool { True }.signature ~~ :(Object --> Bool),
    'a parameter typed with a class doing the role is accepted';
ok  sub (Object $) returns Bool { True }.signature ~~ :(Object --> Bool),
    'the role itself is accepted';
ok  sub (Derived $) { }.signature ~~ :(Base),
    'a subclass parameter is accepted by its parent';
nok sub (Base $) { }.signature ~~ :(Derived),
    'a parent parameter is not accepted by a subclass matcher';
nok sub (Int $) returns Bool { True }.signature ~~ :(Object --> Bool),
    'an unrelated type is still rejected';
nok sub ($x) returns Bool { True }.signature ~~ :(Object --> Bool),
    'an untyped parameter is wider than the role';
nok sub (Object $, $y) returns Bool { True }.signature ~~ :(Object --> Bool),
    'an extra positional still fails';

subset ValidateCallback of Callable where { .signature.params && .signature ~~ :(Object --> Bool) };
my ValidateCallback @validators;
lives-ok { @validators.push: sub (ObjectOne $) returns Bool { True } },
    'a typed array of such a subset accepts the callback';
is @validators.elems, 1, 'and holds it';
