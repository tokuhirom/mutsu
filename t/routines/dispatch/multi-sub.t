use Test;
plan 4;

multi sub greet() {
    return "hello";
}
multi sub greet($name) {
    return "hello, " ~ $name;
}
multi sub greet($first, $last) {
    return "hello, " ~ $first ~ " " ~ $last;
}

is greet(), "hello", 'multi sub with 0 args';
is greet("world"), "hello, world", 'multi sub with 1 arg';
is greet("John", "Doe"), "hello, John Doe", 'multi sub with 2 args';

# multi without explicit 'sub' keyword
multi factorial(0) { return 1; }
multi factorial($n) { return $n; }
is factorial(5), 5, 'multi without sub keyword';
