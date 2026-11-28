use Test;

# A `:D` attribute `is required` that was passed an undefined value fails the
# assignment type check; one that was not passed at all is the required error.
# The constructor asks its named arguments by name (`ProvidedNamed`).

plan 8;

role R { has Str:D $.value is required; has Str $.note }
class C does R { }

is C.new(:value<v>).value, 'v', 'provided and defined';
throws-like { C.new(:value(Str)) }, X::TypeCheck::Assignment,
    'provided undefined is a type check failure';
throws-like { C.new(:note<n>) }, X::Attribute::Required,
    'not provided is the required error';
my Str $undef;
throws-like { C.new(:value($undef)) }, X::TypeCheck::Assignment,
    'an undefined variable is provided too';

class D {
    has Int:D $.n is required;
    method new(*%a) { self.bless(|%a) }
}
is D.new(:n(3)).n, 3, 'through a user new and bless';
throws-like { D.new(:n(Int)) }, X::TypeCheck::Assignment,
    'provided undefined through bless';
throws-like { D.new() }, X::Attribute::Required, 'missing through bless';

class E { has Int:D $.n = 1 }
is E.new.n, 1, 'a :D attribute with a default needs no argument';
