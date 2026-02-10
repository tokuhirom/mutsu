use Test;
plan 3;

role Greetable {
    method greet(Str $name) { return "Hello, " ~ $name }
}

class Person {
    does Greetable;
}

my $p = Person.new();
is $p.greet("Raku"), "Hello, Raku", 'role method composed';

role CounterRole {
    has $.count = 0;
    method inc() { $!count = $!count + 1; return $!count }
}

class Counter {
    does CounterRole;
}

my $c = Counter.new();
is $c.count, 0, 'role attribute composed';
is $c.inc(), 1, 'role method uses attribute';
