use Test;

# ADR-8751 stage 2: a block-scoped `use` binds an imported multi family whose
# proto has a body beyond `{*}` (or a signature beyond a capture) in the
# block's lexical import set. A call through it runs the proto, whose `{*}`
# reaches the imported candidates; a callee written outside the block does not
# see the family; `&f` taken out of the block keeps the proto body.
plan 10;

use lib 't/lib';

sub outside { (try EVAL 'uibnp(1)') // 'not visible' }
sub outside-sig { (try EVAL 'uibnp-sig(1)') // 'not visible' }

{
    use UseInBlockNontrivialProto;
    is uibnp(1), 'proto:int 1', 'the proto body runs around the Int candidate';
    is uibnp('a'), 'proto:str a', 'and around the Str candidate';
    is uibnp-sig(2), 'sig 2', 'a signature-only proto dispatches to its candidate';
    is uibnp-rec(2), '<2<1<z>>>', 'a candidate calling its family re-enters the proto';
    is outside(), 'not visible', 'a callee outside the block does not see the family';
    is outside-sig(), 'not visible', 'nor the signature-only one';
}

my &taken = do { use UseInBlockNontrivialProto; &uibnp };
is taken(3), 'proto:int 3', '&f taken out of the block keeps the proto body';

my &closure = do { use UseInBlockNontrivialProto; -> $x { uibnp($x) } };
is closure('b'), 'proto:str b', 'a closure created in the block keeps it too';

my &sig-closure = do { use UseInBlockNontrivialProto; -> $x { uibnp-sig($x) } };
is sig-closure(4), 'sig 4', 'and the signature-only family';

is outside(), 'not visible', 'still not visible after the blocks';
