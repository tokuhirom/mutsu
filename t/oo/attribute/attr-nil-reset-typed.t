use v6;
use Test;

plan 7;

# Assigning Nil to a typed scalar attribute resets it to the attribute's
# own type object, not Any (URI's `$!authority = Nil`).
class Foo {}
class C {
    has Foo $.f is rw;
    multi method get(C:D: --> Foo) { $!f }
    method reset() { $!f = Nil }
}

my $c = C.new;
ok $c.get === Foo, 'unset typed attribute is its type object';
$c.f = Foo.new;
ok $c.get.defined, 'assigned attribute is defined';
$c.reset;
ok $c.get === Foo, '$!f = Nil resets to the type object';
lives-ok { $c.get }, 'a --> Foo return constraint accepts the reset value';

# Untyped attribute still resets to Any.
class U {
    has $.u is rw;
    method reset() { $!u = Nil }
}
my $u = U.new(u => 42);
$u.reset;
ok $u.u === Any, 'untyped attribute resets to Any';

# Nested class type: the reset type object dispatches methods (`.= new`).
class Outer {
    class Inner { has $.x }
    has Inner $.i is rw;
    method reset() { $!i = Nil }
    method mk() { $!i .= new(x => 1) }
}
my $o = Outer.new;
$o.reset;
$o.mk;
is $o.i.x, 1, 'reset nested-class attribute can .= new';

# Inherited typed attribute resets via MRO walk (the reset method lives on
# the parent; self is the Derived instance, so the type lookup walks the MRO).
class Base {
    has Foo $.g is rw;
    method reset() { $!g = Nil }
}
class Derived is Base { }
my $d = Derived.new(g => Foo.new);
$d.reset;
ok $d.g === Foo, 'inherited typed attribute resets to its type object';
