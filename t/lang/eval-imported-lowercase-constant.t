use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

plan 2;

# An EVAL'd `use` of a module exporting a lowercase `constant` must make the
# bare term resolvable, as uppercase ones already were (#12574).
is EVAL(q[use ReturnSpecAlias; answer]), 5, 'EVAL resolves an imported lowercase constant';
is EVAL(q[use ReturnSpecAlias; answer + 1]), 6, 'and it is usable in an expression';
