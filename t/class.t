use Test;
plan 25;

# Basic class declaration and instantiation
class Point {
    has $.x;
    has $.y;
}

my $p = Point.new(x => 3, y => 4);
ok $p.defined, "instance is defined";
is $p.x, 3, "public attribute x";
is $p.y, 4, "public attribute y";

# .WHAT returns (ClassName)
is $p.WHAT, "(Point)", ".WHAT";

# .isa checks
ok $p.isa("Point"), ".isa with own class";

# .gist / .Str
is $p.gist, "Point()", ".gist";
is $p.Str, "Point()", ".Str";

# .raku / .perl
is $p.raku, "Point.new()", ".raku";
is $p.perl, "Point.new()", ".perl";

# Attribute defaults
class WithDefault {
    has $.name;
    has $.count = 0;
}

my $wd = WithDefault.new(name => "test");
is $wd.name, "test", "named arg sets attribute";
is $wd.count, 0, "attribute default value";

# Private attributes
class Secret {
    has $!hidden = 42;
    method reveal() { $!hidden }
}

my $s = Secret.new;
is $s.reveal, 42, "private attribute accessible in method";

# Methods
class Dog {
    has $.name;
    method speak() { "Woof! I am " ~ $.name }
}

my $d = Dog.new(name => "Rex");
is $d.speak, "Woof! I am Rex", "method accessing public attribute";

# .clone
my $p2 = $p.clone(x => 10);
is $p2.x, 10, ".clone overrides attribute";
is $p2.y, 4, ".clone keeps other attributes";
is $p.x, 3, "original unchanged after clone";

# Inheritance
class Animal {
    has $.name;
    method greet() { "I am " ~ $.name }
}

class Cat is Animal {
    has $.color;
    method describe() { $.name ~ " is " ~ $.color }
}

my $c = Cat.new(name => "Whiskers", color => "black");
is $c.name, "Whiskers", "inherited attribute";
is $c.color, "black", "child attribute";
is $c.greet, "I am Whiskers", "inherited method";
is $c.describe, "Whiskers is black", "child method";
ok $c.isa("Cat"), "isa own class";
ok $c.isa("Animal"), "isa parent class";

# Method with parameters
class Calculator {
    has $.base;
    method add($n) { $.base + $n }
}

my $calc = Calculator.new(base => 10);
is $calc.add(5), 15, "method with parameter";

# Multiple instances are independent
my $p3 = Point.new(x => 1, y => 2);
my $p4 = Point.new(x => 100, y => 200);
is $p3.x, 1, "first instance independent";
is $p4.x, 100, "second instance independent";
