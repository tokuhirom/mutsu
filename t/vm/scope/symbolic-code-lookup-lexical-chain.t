use v6;
use Test;
use lib 't/lib';

# A by-name routine lookup (`::('&name')`, `&::('name')`) sees the imports of
# the blocks lexically around it, not those of the blocks open in its callers
# (#14283).

plan 8;

sub c1 { (try ::('&symlook-fx')()) // 'not visible' }
sub c2 { (try &::('symlook-fx')()) // 'not visible' }
my @result;
{
    use SymLookExport;
    @result = [
        c1(),
        c2(),
        ::('&symlook-fx')(),
        &::('symlook-fx')(),
        (-> { ::('&symlook-fx')() })(),
        (do { my &inner = -> { &::('symlook-fx')() }; inner() }),
    ];
    my &keep = -> { ::('&symlook-fx')() };
    @result.push(keep());
}
is @result[0], 'not visible', 'a routine called from the block does not see ::(\'&name\')';
is @result[1], 'not visible', 'nor &::(\'name\')';
is @result[2], 'fx', 'the block itself sees ::(\'&name\')';
is @result[3], 'fx', 'the block itself sees &::(\'name\')';
is @result[4], 'fx', 'a pointy block inside the block sees it';
is @result[5], 'fx', 'a lookup in a closure inside the block sees it';
is @result[6], 'fx', 'a closure keeps the lookup after it returns';
is (try ::('&symlook-fx')()) // 'not visible', 'not visible', 'outside the block it is gone';
