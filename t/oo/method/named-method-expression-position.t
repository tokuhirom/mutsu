use Test;

# A named `method NAME (...) { }` written in expression position is still a
# declaration: inside a package body it installs NAME as a method of that
# package, and the expression evaluates to the method object (#9474).

plan 12;

class A {
    has @.x = method TOP ($/) { 1 };
    method go { self.TOP(1) }
}
is A.new.go, 1, 'a method declared in an attribute initializer is a method of the class';
is A.new.x.elems, 1, 'the initializer receives the method object';
isa-ok A.new.x[0], Method, 'that object is a Method';
is A.new.x[0].name, 'TOP', 'carrying its declared name';

grammar G { token TOP { \d+ } }
class Act {
    has @.ignored = method TOP ($/) { make +$/ * 2 };
}
is G.parse('21', actions => Act.new).made, 42, 'a grammar finds the hoisted action method';

role R { has $.c = method rr { 7 } }
class B does R { }
is B.new.rr, 7, 'a role body hoists the method too';
is B.new.c.name, 'rr', 'and the role attribute holds the method';

class C { has $.s = submethod sm { 5 } }
is C.new.sm, 5, 'the submethod spelling is hoisted as well';
isa-ok C.new.s, Submethod, 'and evaluates to a Submethod';

my $m = method foo ($x) { $x * 2 };
is $m(Any, 21), 42, 'in the mainline the expression yields a callable method';
is $m.name, 'foo', 'named as declared';

my %h = method => 1;
is %h<method>, 1, '`method =>` is still a pair key';
