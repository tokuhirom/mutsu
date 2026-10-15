use v6.d;
use Test;

# A class HOW with a role mixed in (`$how does Role`, what AttrX::Mooish does to
# the HOW of a class with a mooish attribute) still answers the
# Metamodel::MethodContainer protocol, `declares_method` included.
plan 6;

role Probe {
    method declared(Mu \type, Str $name) { self.declares_method(type, $name) }
    method found(Mu \type, Str $name)    { self.find_method(type, $name).defined }
}

class Foo { method bar { 1 } }
class Base { method inherited { 1 } }
class Child is Base { has $.attr; }

my $foo-how = Foo.HOW;
$foo-how does Probe;

ok $foo-how.declared(Foo, 'bar'), 'declares_method sees a declared method';
nok $foo-how.declared(Foo, 'nope'), 'declares_method refuses an absent name';
ok $foo-how.found(Foo, 'bar'), 'find_method still works through the mixin';

my $child-how = Child.HOW;
$child-how does Probe;
nok $child-how.declared(Child, 'inherited'), 'declares_method does not walk the MRO';
ok $child-how.declared(Child, 'attr'), 'declares_method sees an accessor';
ok $child-how.found(Child, 'inherited'), 'find_method walks the MRO';
