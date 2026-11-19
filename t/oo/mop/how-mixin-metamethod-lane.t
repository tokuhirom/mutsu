use Test;

# A metamethod call on a class whose metaobject has a role mixed in
# (`Foo.HOW does R`, what AttrX::Mooish does to every class it touches) or is
# a user subclass of `Metamodel::ClassHOW` answers from the builtin
# metamethod when nothing in front of it provides one, and from the role or
# the subclass when it does.

plan 21;

my role Plain { method helper-only { 'plain' } }
my role OwnFind {
    method find_method(Mu $obj, $name, *%) { 'role-find:' ~ $name }
}
my role Inner { method find_method(Mu $obj, $name, *%) { 'inner-find:' ~ $name } }
my role Outer does Inner { }

my class A { method m { 'A.m' } }
A.HOW does Plain;

is A.^find_method('m').name, 'm', 'role mixin without find_method: builtin answer';
is A.^find_method('m', :no_fallback(1)).name, 'm', '... with :no_fallback';
nok A.^find_method('nope', :no_fallback(1)).defined, '... a missing method is undefined';
my $a = A.new;
is $a.^find_method('m')($a), 'A.m', '... on an instance held in a variable';
is $a.^name, 'A', '.^name on the instance';
is A.^mro.map(*.^name).join(','), 'A,Any,Mu', '.^mro';
is A.HOW.helper-only, 'plain', 'the mixed-in role still answers its own method';

my class B { method m { 'B.m' } }
B.HOW does OwnFind;
is B.^find_method('m'), 'role-find:m', 'a mixed-in find_method answers first';
is B.^find_method('x', :no_fallback(1)), 'role-find:x', '... with :no_fallback';

my class C { method m { 'C.m' } }
C.HOW does Outer;
is C.^find_method('m'), 'inner-find:m', 'a find_method from a role the mixed-in role does answers first';

class SubHOW is Metamodel::ClassHOW { }
class SubFindHOW is Metamodel::ClassHOW {
    method find_method(Mu $obj, $name, *%h) { $name eq 'm' ?? 'subclass-find:m' !! callsame }
}

my $D := SubHOW.new_type(:name<D>);
$D.^add_method('m', my method dm { 'D.m' });
$D.^compose;
is $D.^find_method('m')($D), 'D.m', 'metaclass subclass without find_method: builtin answer';
nok $D.^find_method('nope', :no_fallback(1)).defined, '... a missing method is undefined';
is $D.HOW.^name, 'SubHOW', '... the metaclass is the subclass';

$D.HOW does Plain;
is $D.^find_method('m')($D), 'D.m', 'subclass metaclass with a role mixed in: builtin answer';
is $D.HOW.helper-only, 'plain', '... and the role still answers its own method';

my $E := SubFindHOW.new_type(:name<E>);
$E.^add_method('m', my method em { 'E.m' });
$E.^compose;
is $E.^find_method('m'), 'subclass-find:m', 'a metaclass subclass find_method answers';
$E.HOW does Plain;
is $E.^find_method('m'), 'subclass-find:m', '... also with a role mixed in';

class G { method m { 'G.m' }; method look($n) { self.^find_method($n) } }
G.HOW does Plain;
is G.new.look('m').name, 'm', 'self.^find_method inside a method';
is G.look('m').name, 'm', '... on the type object';

my class J { has $.v; method m { 'J.m' } }
J.HOW does Plain;
is J.^attributes.map(*.name).join(','), '$!v', '.^attributes through the mixin';
is J.^methods(:local).map(*.name).grep(* ne 'POPULATE').sort.join(','), 'm,v', '.^methods(:local) through the mixin';
