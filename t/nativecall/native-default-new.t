use v6;
use Test;

# Native default construction (`Foo.new(...)` for simple user classes) is built
# directly in the VM. These cases exercise that path and its fall-through.

plan 18;

# Simple class, named args
class Point { has $.x; has $.y; }
my $p = Point.new(x => 3, y => 4);
is $p.x, 3, 'named arg x';
is $p.y, 4, 'named arg y';

# Missing args default to (Any) / undefined
my $q = Point.new(x => 1);
is $q.x, 1, 'partial named arg';
nok $q.y.defined, 'missing attribute is undefined';

# Inheritance chain (Dog is Animal) — both classes simple
class Animal { has $.name; has $.legs; }
class Dog is Animal { has $.breed; }
my $d = Dog.new(name => "Rex", legs => 4, breed => "Lab");
is $d.name, 'Rex', 'inherited attr name';
is $d.legs, 4, 'inherited attr legs';
is $d.breed, 'Lab', 'own attr breed';
isa-ok $d, Animal, 'subclass isa parent';

# Literal default
class Counter { has $.count = 0; }
is Counter.new.count, 0, 'literal default applied';
is Counter.new(count => 5).count, 5, 'named arg overrides default';

# Default referencing earlier attribute via self
class Sum { has $.a = 2; has $.b = 3; has $.total = self.a + self.b; }
is Sum.new.total, 5, 'self-referencing default';
is Sum.new(a => 10).total, 13, 'self-referencing default with override';

# .new on an instance delegates to its class
my $p2 = $p.new(x => 7, y => 8);
is $p2.x, 7, 'instance.new delegates to class';

# Type identity
is $p.WHAT.^name, 'Point', 'WHAT type name';
ok $p ~~ Point, 'smartmatch against type';

# Fall-through cases (NOT native-eligible) still work correctly:

# class with custom BUILD
class WithBuild {
    has $.v;
    submethod BUILD(:$x = 0) { $!v = $x * 2 }
}
is WithBuild.new(x => 5).v, 10, 'custom BUILD still runs';

# class with custom new
class WithNew {
    has $.v;
    method new($n) { self.bless(v => $n + 100) }
}
is WithNew.new(7).v, 107, 'custom new still runs';

# typed attribute (ineligible) constructs and type-checks
class Typed { has Int $.n; }
is Typed.new(n => 42).n, 42, 'typed attribute constructs';
