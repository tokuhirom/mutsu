use v6;
use Test;

# `.flat` itemization depth: an Array's elements live in Scalar containers,
# so flat does not descend into them — a List/Array element of an Array
# stays single (and itemized in .raku). List elements of a List flatten.
# Found by the doc-diff sweep (Language/list.rakudoc [8]/[10]/[11]).

plan 10;

{
    my @l := 2, (3, 4);
    is-deeply (1, @l, 5).flat.List, (1, 2, 3, 4, 5),
        'bound List flattens fully';
    my @a = 2, (3, 4);
    is (1, @a, 5).flat.elems, 4,
        'Array element List stays single under flat';
    is (1, @a, 5).flat[2].gist, '(3 4)',
        'the un-flattened element is the inner list';
    is @a.flat.raku, '(2, $(3, 4)).Seq', 'Array.flat itemizes List elements';
}

is ((1, 2), $(3, 4)).flat.raku, '(1, 2, $(3, 4)).Seq',
    'List flattens, itemized element stays (doc example)';
is [(1, 2), $(3, 4)].flat.raku, '($(1, 2), $(3, 4)).Seq',
    'Array.flat keeps both elements itemized (doc example)';
is (0, [(1, 2), $(3, 4)], 5).flat.raku, '(0, $(1, 2), $(3, 4), 5).Seq',
    'Array inside a List spills itemized elements (doc example)';

is [[1, 2], [3, 4]].flat.raku, '($[1, 2], $[3, 4]).Seq',
    'Array elements of an Array stay itemized arrays';
is (1, ((2, 3), 4)).flat.List, (1, 2, 3, 4),
    'nested Lists in List context flatten fully';
is join(",", [1, (2, 3)]), '1,2 3',
    'join respects the itemization depth';

done-testing;
