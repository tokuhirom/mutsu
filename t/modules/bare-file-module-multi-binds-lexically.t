use Test;

# ADR-8751 stage 2: the multi family a block-scoped `use` imports from a
# bare-file module (no `unit module`, so its candidates register under
# GLOBAL) binds in the block's lexical import set like any module's. Inside
# the block the import hides the unit's own family; a callee written outside
# the block keeps seeing the unit's family; a closure taken in the block
# keeps the import.
plan 5;

use lib 't/lib';

multi sub bfml-f($x) { "main $x" }
sub bfml-callee { bfml-f(1) }

{
    use BareFileMultiLexical;
    is bfml-f(1), 'BF int 1', 'inside the block the imported family answers';
    is bfml-f('a'), 'BF str a', 'with every candidate';
    is bfml-callee(), 'main 1', 'a callee written outside the block does not see it';
}

my &keep = do { use BareFileMultiLexical; -> $x { bfml-f($x) } };
is keep(2), 'BF int 2', 'a closure taken in the block keeps the import';
is bfml-f(1), 'main 1', 'after the block the unit\'s own family answers';
