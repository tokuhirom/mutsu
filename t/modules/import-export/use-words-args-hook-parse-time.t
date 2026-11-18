use lib 't/lib';
use Test;

# A `use` whose arguments are angle-bracket words is as literal as a quoted
# one: the module with a `sub EXPORT` loads at parse time, so the operators
# its load binds into `EXPORT::MANDATORY` are known to the parser below
# (annotations, #18091).

plan 2;

use ExportMandatoryHook <x y>;

is 1 hook-cat 2, '1+2', 'a MANDATORY operator of a `use M <x y>` parses';
is (1 [hook-cat] 2), '1+2', '... in the bracketed form too';
