use v6;
use Test;

# The importer's parse learns the value terms and operators a `sub EXPORT`
# hook declares locally (see export-hook-value-term.t). Only declarations the
# returned `Map` can see count: the hook body's own scope, and a bare block
# that ends the body (its value is the hook's return value). A declaration in
# an earlier bare block is private to that block.

plan 3;

use lib 't/lib';

{
    use ExportHookInnerBlock;
    # `twice` stays a routine: the `my \twice` in the hook's earlier block is
    # not an export, so `twice 5` is a listop call, not two terms in a row.
    is (twice 5), 10, 'a sigilless term in an earlier block of the hook is not exported';
}

{
    use ExportHookTailBlock;
    ok oui, 'a value term declared in the block ending the hook is exported';
    is (1 puis 2), '1,2', 'an operator declared in the block ending the hook is exported';
}
