use Test;
use lib 't/lib';

plan 4;

# `use` is compile-time in Raku: a routine it imports is late-bound for a call
# written above it, when the name meant nothing before.
is late-greet("x"), 'hello x', 'a call above the use reaches the imported routine';
is late-pair(2, :b(3)), 5, 'with a named argument';

is &late-greet.name, 'late-greet', 'the code variable above the use';

sub local-first { late-greet("y") }
is local-first(), 'hello y', 'a routine body is late-bound as before';

use LateImportGreet;
