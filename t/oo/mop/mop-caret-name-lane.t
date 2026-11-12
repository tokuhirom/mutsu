use Test;

# `.^name` on an ordinary user class is answered by a lane ahead of the
# general method-call probes (#17552, found in Red, whose `Red::AST.WHICH`
# calls `self.^name` for every hash key). These pin what the lane must still
# leave to the general path: a `method ^name`, an inherited or role-supplied
# one, a mixin, a renamed type and a metaclass instance.

plan 13;

class A { has @.args = 1, 2; method who { self.^name } }
my $a = A.new;
is $a.^name, 'A', 'instance of a plain class';
is A.^name, 'A', 'type object of a plain class';
is $a.who, 'A', 'self.^name inside a method';

class Outer::Inner { }
is Outer::Inner.new.^name, 'Outer::Inner', 'qualified class name';

class B { method ^name($) { "Custom" } }
is B.new.^name, 'Custom', 'a declared ^name metamethod wins on an instance';

class C is B { }
is C.new.^name, 'Custom', 'an inherited ^name metamethod wins';

role R { }
my $m = A.new but R;
is $m.^name, 'A+{R}', 'a mixin keeps its composed name';

class D { }
D.^set_name("Renamed");
is D.new.^name, 'Renamed', 'set_name is visible through .^name';

is Metamodel::ClassHOW.new.^name, 'Perl6::Metamodel::ClassHOW', 'a metaclass instance';

class E is Exception { }
is E.new.^name, 'E', 'a subclass of a core class';

my @names = (^50).map({ A.new.^name });
is @names.unique.join, 'A', 'repeated calls agree';

is $a.HOW.name($a), 'A', 'the HOW answers the same name';

class F { has $.x }
is F.new(x => 1).^name, 'F', 'class with an attribute';
