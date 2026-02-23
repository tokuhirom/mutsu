use Test;
plan 8;

# Multi dispatch with type constraints
multi sub describe(Int $x) { "integer $x" }
multi sub describe(Str $x) { "string $x" }

is describe(42), 'integer 42', 'multi dispatch selects Int variant';
is describe('hello'), 'string hello', 'multi dispatch selects Str variant';

# Type constraints with Rat
multi sub numtype(Int $x) { "int" }
multi sub numtype(Rat $x) { "rat" }

is numtype(5), 'int', 'Int matches Int constraint';
is numtype(3.14), 'rat', 'Rat matches Rat constraint';

# Bool type constraint
multi sub boolcheck(Bool $x) { "bool" }
multi sub boolcheck(Int $x) { "int" }

is boolcheck(True), 'bool', 'Bool matches Bool constraint';
is boolcheck(7), 'int', 'Int matches Int constraint (not Bool)';

# Multiple parameters with types
multi sub add(Int $a, Int $b) { $a + $b }
multi sub add(Str $a, Str $b) { $a ~ $b }

is add(3, 4), 7, 'multi add(Int, Int) works';
is add('foo', 'bar'), 'foobar', 'multi add(Str, Str) works';
