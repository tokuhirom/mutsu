use v6;
use Test;

# A multi-dimensional hash subscript must recurse into a Pair value
# associatively: `%h{"a","b";"k"}` where each `%h{key}` is a Pair should
# index that Pair by "k". Previously the Pair was treated as a scalar leaf
# and the trailing key collapsed to Nil.

plan 4;

# The doc example (Language/subscripts.rakudoc): pair-valued hash.
{
    my %pantheon = %('Baldr' => 'consort' => 'Nanna',
                     'Bragi' => 'consort' => 'Iðunn',
                     'Nótt'  => 'consort' => 'Dellingr');
    is-deeply %pantheon{'Bragi', 'Nótt'; 'consort'}.List, ('Iðunn', 'Dellingr'),
        'multidim subscript recurses into pair values by key';
}

# Whatever on the first dimension, then a key into each pair value.
{
    my %p = %(a => x => 1, b => x => 2, c => x => 3);
    is-deeply %p{*; 'x'}.sort.List, (1, 2, 3),
        'whatever first dim then a key into each pair value';
}

# Nested-hash values still work the same way (no regression).
{
    my %h = a => (x => 10, y => 20).Hash, b => (x => 30, y => 40).Hash;
    is-deeply %h{'a', 'b'; 'x'}.List, (10, 30), 'multidim into nested hash values';
}

# Array-of-lists multidim subscript still works.
{
    my @a = (1, 2, 3), (4, 5, 6);
    is-deeply @a[0, 1; 1].List, (2, 5), 'array multidim slice unaffected';
}
