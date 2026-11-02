use v6;
use lib $*PROGRAM.parent(3).add('lib-tail-block-scope');
use Test;

plan 3;

use TailBlockScope;

# The module declares `multi infix:<->` in its last bare block. The block's
# routines are lexical to it: the module's own `plain` keeps the core `-`,
# while the importer gets the exported candidate.
is plain(5, 3), 2, "a module's routine does not see a multi from its tail block";
is 5 - 3, 'user', 'the importer sees the exported candidate';
{
    use TailBlockScope;
    is plain(9, 4), 5, 'also inside an importing block';
}
