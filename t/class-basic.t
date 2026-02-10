use Test;

plan 14;

# Basic class declaration and instantiation
class Point {
    has $.x;
    has $.y;
}

my $p = Point.new(x => 3, y => 4);
ok $p.defined, 'instance is defined';
is $p.x, 3, 'public attribute x accessor';
is $p.y, 4, 'public attribute y accessor';
is $p.WHAT, '(Point)', '.WHAT returns class name';

# Method declaration
class Greeter {
    has $.name;
    method greet() {
        return "Hello, $!name!";
    }
}

my $g = Greeter.new(name => 'World');
is $g.greet(), 'Hello, World!', 'method call works';

# Private attribute (no accessor)
class Secret {
    has $!hidden;
    has $.visible;
    method reveal() {
        return $!hidden;
    }
}

my $s = Secret.new(hidden => 42, visible => 'yes');
is $s.visible, 'yes', 'public attribute is accessible';
is $s.reveal(), 42, 'private attribute accessible via method';

# Default values
class WithDefault {
    has $.color = 'red';
    has $.size = 10;
}

my $d = WithDefault.new();
is $d.color, 'red', 'default value for attribute';
is $d.size, 10, 'default value for numeric attribute';

my $d2 = WithDefault.new(color => 'blue');
is $d2.color, 'blue', 'override default with constructor arg';
is $d2.size, 10, 'non-overridden default preserved';

# Method with parameters
class Calculator {
    has $.value = 0;
    method add($n) {
        return $!value + $n;
    }
}

my $c = Calculator.new(value => 10);
is $c.add(5), 15, 'method with parameter';

# Inheritance
class Animal {
    has $.name;
    method speak() {
        return "...";
    }
}

class Dog is Animal {
    method speak() {
        return "Woof!";
    }
}

my $dog = Dog.new(name => 'Rex');
is $dog.name, 'Rex', 'inherited attribute accessor';
is $dog.speak(), 'Woof!', 'overridden method';

done-testing;
