use Test;

# `.sort` on a Set/Bag/Mix used to fall out of the VM's native sort gate into
# the tree-walking interpreter, which cannot invoke a compiled WhateverCode
# mapper: every sort key came back Nil, all elements compared equal, and the
# result was the container's unordered iteration order (so the output differed
# from run to run). Pin the ordering for each setty type and callable shape.

plan 11;

my $bag = bag <a b a c a b>;   # a => 3, b => 2, c => 1

is $bag.sort(*.value).map(*.key).join(','), 'c,b,a',
    'Bag.sort(*.value) is ascending by weight';
is $bag.sort(-*.value).map(*.key).join(','), 'a,b,c',
    'Bag.sort(-*.value) is descending by weight';
is $bag.sort({ $^x.value <=> $^y.value }).map(*.key).join(','), 'c,b,a',
    'Bag.sort with a two-arg comparator';
is $bag.sort(*.key).map(*.key).join(','), 'a,b,c',
    'Bag.sort(*.key) is ascending by key';
is $bag.sort(-*.value).head(2).map(*.key).join(','), 'a,b',
    'the sorted Seq keeps its order through .head';

# Repeat the WhateverCode sort: an unordered-iteration result would not be
# stable across repetitions within one process.
my @runs = (^5).map({ $bag.sort(-*.value).map(*.key).join(',') }).unique;
is @runs.elems, 1, 'Bag.sort(-*.value) is deterministic';

my $set = set <c a b>;
is $set.sort(*.key).map(*.key).join(','), 'a,b,c',
    'Set.sort(*.key) is ascending by key';
is $set.sort({ $^y.key leg $^x.key }).map(*.key).join(','), 'c,b,a',
    'Set.sort with a two-arg comparator';

my $mix = (a => 1.5, b => 3.5, c => 2.5).Mix;
is $mix.sort(*.value).map(*.key).join(','), 'a,c,b',
    'Mix.sort(*.value) is ascending by weight';
is $mix.sort(-*.value).map(*.key).join(','), 'b,c,a',
    'Mix.sort(-*.value) is descending by weight';

my %h = a => 1, b => 3, c => 2;
is %h.sort(-*.value).map(*.key).join(','), 'b,c,a',
    'Hash.sort(-*.value) still works (the shape this was already right for)';

done-testing;
