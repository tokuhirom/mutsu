use Test;

plan 12;

# A class's attribute type constraints, attribute list and same-named-attribute
# slots are memoized per class shape; each must follow a change of that shape.

class Typed { has Int $.n is rw = 1; submethod BUILD(:$n) { $!n = $n // 1 } }
is Typed.new(n => 5).n, 5, 'a typed attribute set by BUILD';
throws-like { Typed.new(n => 'x') }, X::TypeCheck, 'the type constraint is enforced';
is Typed.new(n => 6).n, 6, 'and keeps being enforced the second time';

# ^add_attribute changes the shape after the first construction.
class Grows { has $.a = 1; submethod BUILD(:$a) { $!a = $a // 1 } }
is Grows.new.^attributes.elems, 1, 'one attribute before';
Grows.^add_attribute(Attribute.new(:name<$!b>, :type(Int), :package(Grows), :has_accessor));
Grows.^compose;
is Grows.new.^attributes.elems, 2, 'the new attribute is seen after ^add_attribute';

# The same private name in parent and child needs a slot per class.
class Base { has $!x = 'base'; method base-x { $!x } }
class Derived is Base { has $!x = 'derived'; method derived-x { $!x } }
my $d = Derived.new;
is $d.base-x, 'base', 'a parent keeps its own private attribute';
is $d.derived-x, 'derived', 'a child keeps its own';
my $d2 = Derived.new;
is $d2.base-x ~ $d2.derived-x, 'basederived', 'and again for the next object';

# Smileys and defaults keep applying on every construction.
class Smiley { has Int:D $.v = 0; submethod BUILD(:$v) { $!v = $v // 0 } }
is Smiley.new(v => 3).v, 3, ':D attribute accepts a defined value';
is Smiley.new.v, 0, ':D attribute takes its default';
my $count = 0;
for ^20 { $count++ if Typed.new(n => $_).n == $_ }
is $count, 20, 'twenty constructions of one class agree';
is Typed.new(n => 7).n, 7, 'the cached shape is still right afterwards';
