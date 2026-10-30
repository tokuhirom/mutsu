use Test;

plan 4;

# `does R[Array, Foo::Bar]` names a qualified type: it is not a forwarded
# `::T` capture, so the parent role's type parameters stay bound for its
# submethods and methods.
my class Foo::Bar { }
my role R[::T, ::H] { has $.x; submethod BUILD { $!x = T.new }; method m { T.new } }

my role Q does R[Array, Foo::Bar] { }
my class B does Q { }
is B.new.x.raku, '[]', 'BUILD sees the bound parameter';
is B.new.m.raku, '[]', 'a method sees it too';

my role Q2 does R[Array, Metamodel::PackageHOW] { }
my class B2 does Q2 { }
is B2.new.x.raku, '[]', 'a core qualified type argument';

my role Q3 does R[Array, Int] { }
my class B3 does Q3 { }
is B3.new.x.raku, '[]', 'an unqualified argument still works';
