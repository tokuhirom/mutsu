use Test;
plan 7;

# &foo reference to a sub
sub greet($name) { "Hello, $name" }
my $ref = &greet;
is $ref('World'), 'Hello, World', '&sub creates callable reference';

# &foo direct call
is &greet('Raku'), 'Hello, Raku', '&sub can be called directly';

# my &f = &sub
sub double($x) { $x * 2 }
my &f = &double;
is &f(5), 10, 'my &f = &sub works';

# Pass code ref as argument
sub apply($func, $val) { $func($val) }
sub square($x) { $x * $x }
is apply(&square, 4), 16, 'pass &sub as argument';

# Store in scalar, call via scalar
my $fn = &square;
is $fn(3), 9, 'scalar holding code ref is callable';
is &$fn(3), 9, 'scalar code ref can be invoked via &$var()';

# Reference to multi sub
multi sub show(Int $x) { "int:$x" }
multi sub show(Str $x) { "str:$x" }
is &show(42), 'int:42', '&multi-sub reference works';
