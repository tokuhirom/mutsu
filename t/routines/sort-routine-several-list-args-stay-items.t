use v6;
use Test;

# `sort` takes `+values`: only a lone positional is flattened. With several,
# each array/list is one item to order. Found with Math::Sequences'
# `strict-partitions` test. Identical under raku.

plan 6;

is-deeply sort([10], [9,1], [8,2]).list, ([8,2], [9,1], [10]), 'several arrays are ordered as items';
is-deeply sort((3,1), (2,0)).list, ((2,0), (3,1)), 'several lists are ordered as items';
is-deeply sort(1, [3,0], 2).list, (1, 2, [3,0]), 'an array among scalars stays whole';
is-deeply sort([3,1]).list, (1, 3), 'a lone array is still flattened';
is-deeply sort(<b a>, <d c>).list, (<b a>, <d c>), 'two word lists stay two items';
is-deeply sort((3,1), 2).list, (2, (3,1)), 'a list with a scalar';
