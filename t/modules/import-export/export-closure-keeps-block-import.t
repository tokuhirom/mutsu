use Test;

# #8751: a closure created inside `sub EXPORT`'s own `use`-containing block
# runs long after that block's import scope has popped. Rakudo installs a
# `use`'s routines in the enclosing lexical scope, so the closure still sees
# them, and they win over a same-named routine of the importing unit.
plan 3;

use lib 't/lib';
use ExportClosureBlockImportOuter;

is ecbi-wrapped(), 'real from wrapped',
    'a closure keeps the routine its defining block imported';
is ecbi-greet(), 'inner greet from same-name wrapper',
    'a wrapper under the imported routine\'s own name calls the import, not itself';

# The import stays lexical: it is not visible outside the block that made it.
throws-like { EVAL 'ecbi-real()' }, X::Undeclared::Symbols,
    'the inner module\'s routine is not leaked into the importing scope';
