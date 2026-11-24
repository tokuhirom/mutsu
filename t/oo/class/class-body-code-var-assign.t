use Test;

# A class body's `my &t` is a lexical: assigning it in a later body
# statement fills that lexical, not a package variable `&S::t` (#19143).

plan 5;

class A { my &t; &t = sub { 1 }; our $r = t() }
is $A::r, 1, 'my &t declared then assigned: t() calls it';

class B { my &t; &t = sub { 2 }; our $r = &t() }
is $B::r, 2, 'my &t declared then assigned: &t() calls it';

class C { my &t = sub { 3 }; &t = sub { 4 }; our $r = t() }
is $C::r, 4, 'reassigning an initialized my &t replaces it';

class D { my &t; &t = sub { 5 }; method m { t() } }
is D.m, 5, 'a method sees the assigned my &t';

class E { my &t; &t = sub { 6 }; our $pkg = ::('&E::t').defined }
nok $E::pkg, 'the assignment does not create a package &E::t';
