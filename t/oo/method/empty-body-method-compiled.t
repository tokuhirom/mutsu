use Test;

# §B (#3658): empty method/submethod bodies (`submethod BUILD {}`, `method foo {}`)
# now compile to bytecode like any other body, instead of falling through to the
# tree-walk run_instance_method_resolved. After this, that helper's non-delegation
# fallback is reached only for delegation forwarders.

plan 7;

# Empty BUILD submethod — construction still applies attribute defaults.
class A { has $.x = 5; submethod BUILD {} }
is A.new.x, 5, 'empty BUILD submethod: attribute default still applies';

# Empty TWEAK submethod — construction succeeds, named arg bound.
class B { has $.y is rw; submethod TWEAK {} }
is B.new(y => 3).y, 3, 'empty TWEAK submethod: named attribute still bound';

# Empty plain method returns Nil.
class C { method noop {} }
nok C.new.noop.defined, 'empty method returns an undefined value (Nil)';

# Empty BUILD in an inheritance chain — both run, defaults apply.
class Base1 { has $.a = 1; submethod BUILD {} }
class Derived1 is Base1 { has $.b = 2; submethod BUILD {} }
my $d = Derived1.new;
is $d.a, 1, 'empty parent BUILD: inherited default applies';
is $d.b, 2, 'empty child BUILD: own default applies';

# Empty role method composed and called.
role R { method hook {} }
class S does R { }
nok S.new.hook.defined, 'empty composed role method returns Nil';

# A non-empty method on the same class still works alongside the empty one.
class T { method empty {}; method full { 42 } }
is T.new.full, 42, 'non-empty method still works next to an empty one';
