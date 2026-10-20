use Test;

# A qualified type is its own type, not any type whose name is its last
# segment (#14051): `class A {}; module Q { class A {} }` declares two
# unrelated classes. mutsu's type checks equated them, so `A.new ~~ Q::A` was
# True and a `Q::A $x` parameter took an `A`. A bare type name written inside
# a package still names that package's type.

plan 16;

class A { }
module Q { class A { } }

nok A.new ~~ Q::A, 'an outer A is not a Q::A';
nok Q::A.new ~~ A, 'a Q::A is not an outer A';
ok Q::A.new ~~ Q::A, 'a Q::A is a Q::A';

sub takes-qa(Q::A $x) { 'took' }
nok (try takes-qa(A.new)), 'a Q::A parameter rejects an outer A';
is takes-qa(Q::A.new), 'took', '...and takes a Q::A';

sub takes-a(A $x) { 'took' }
nok (try takes-a(Q::A.new)), 'an A parameter rejects a Q::A';

my $typed-ok = try { my Q::A $v = A.new; True };
nok $typed-ok, 'a Q::A variable rejects an outer A';

sub gives-qa(--> Q::A) { A.new }
nok (try gives-qa()), 'a Q::A return type rejects an outer A';

# A bare name in a type position names the type of the package it is
# written in, ahead of an outer type of the same name.
role Marker { method tag { 'outer' } }
class Holder {
    role Marker { method tag { 'inner' } }
    class Impl does Marker { }
    method typed-var { my Marker $m = Impl.new; $m.tag }
    method param(Marker $m) { $m.tag }
    method ret(--> Marker) { Impl.new }
    method smart($x) { $x ~~ Marker }
}
is Holder.typed-var, 'inner', 'a typed variable in a method takes the package role';
is Holder.param(Holder::Impl.new), 'inner', 'a parameter takes the package role';
nok (try Holder.param(class :: does Marker { }.new)), '...and rejects the outer role';
is Holder.ret.tag, 'inner', 'a return type takes the package role';
ok Holder.smart(Holder::Impl.new), 'a smartmatch in a method takes the package role';

# An enum of the class enclosing a role, read from the role's body.
class L {
    enum Level <Off All>;
    role Logger { method lvl(--> Level) { L::All } }
}
class C does L::Logger { }
is C.new.lvl, L::All, 'a return type in a role takes the enclosing class enum';

# A `my`-scoped type keeps its lexical lookup.
class Lex {
    my class Inner { method v { 5 } }
    method m { my Inner $i = Inner.new; $i.v }
}
is Lex.m, 5, 'a my-scoped nested class still types a variable';

# A unit-level package: the rest of the file is inside it.
module U { class Point { has $.x } ; our sub mk(Point $p) { $p.x } }
is U::mk(U::Point.new(:x(3))), 3, 'a package sub takes its own nested class';
