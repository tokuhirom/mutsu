use Test;

# `.flat` on a Seq/List descends one level into its element arrays (they are
# not itemized), while a top-level real Array `[..]` keeps its `[..]` children
# itemized. Regression: the `.flat` method used a separate flattener that passed
# `false` for Seq children, so e.g. `(@a xx 4).flat` left the repeated arrays
# un-flattened (4 elems instead of 12) -- which broke roast S32-str/Collation.t.

plan 8;

my @c = -1, 0, 1;
is (@c xx 4).flat.elems, 12, '(@a xx 4).flat fully descends the repeated arrays';
is-deeply (@c xx 4).flat.List, (-1, 0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1),
    '(@a xx 4).flat values';

# Seq/List of arrays: descend one level.
is ([1, 2], [3, 4]).flat.elems, 4, 'List of two arrays flattens one level';
is ((1, 2), (3, 4)).flat.elems, 4, 'List of two lists flattens';
is (1, (2, 3), 4).flat.elems, 4, 'mixed list flattens nested list';

# Top-level real Array itemizes its `[..]` children: do NOT descend them.
is [[1, 2], [3, 4]].flat.elems, 2, 'real Array does not descend itemized children';
is [1, [2, 3]].flat.elems, 2, 'real Array keeps nested array itemized';

# Plain flat is identity-ish on a flat array.
is [1, 2, 3].flat.elems, 3, 'flat of a flat array is unchanged';
