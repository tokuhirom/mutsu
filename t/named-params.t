use Test;
plan 4;

# Default values
sub greet($name = "World") {
    return "Hello, $name!";
}
is greet(), 'Hello, World!', 'default parameter value';
is greet("Raku"), 'Hello, Raku!', 'override default parameter';

# Multiple params with defaults
sub add($a, $b = 10) {
    return $a + $b;
}
is add(5), 15, 'second param defaults';
is add(5, 3), 8, 'second param overridden';
