use Test;
use lib 't/lib';

# The package grant a block's `use` makes (`M::sub` resolves in the block) is
# the block's own: it ends with the block on every exit path, and it is not
# written back to the enclosing scope (ADR-14140). Checked against rakudo.
plan 8;

{
    use UseInBlockRerun;
    is UseInBlockRerun::uibr-qualified(1), 10, 'the block is granted the module package';
    my &c = -> { UseInBlockRerun::uibr-qualified(2) };
    is c(), 20, 'a closure made in the block keeps the grant';
    is (try EVAL 'UseInBlockRerun::uibr-qualified(3)'), 30, 'an EVAL in the block sees it';
}
dies-ok { EVAL 'UseInBlockRerun::uibr-qualified(1)' }, 'the grant is gone after the block';

# A block that dies leaves no grant behind either.
try { { use UseInBlockRerun; die 'boom' } };
dies-ok { EVAL 'UseInBlockRerun::uibr-qualified(1)' }, 'the grant is gone after a block that died';

# Leaving the block with `last` or `return` ends it too.
for 1 { use UseInBlockRerun; last }
dies-ok { EVAL 'UseInBlockRerun::uibr-qualified(1)' }, 'the grant is gone after a `last`';
sub leaves { use UseInBlockRerun; return 1 }
leaves();
dies-ok { EVAL 'UseInBlockRerun::uibr-qualified(1)' }, 'the grant is gone after a `return`';

# A nested block's grant does not outlive the nested block.
{
    use UseInBlockRerun;
    { use UseInBlockRerun; 1 }
    is UseInBlockRerun::uibr-qualified(4), 40, 'the outer block keeps its grant after an inner one ends';
}
