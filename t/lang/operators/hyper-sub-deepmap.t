use Test;

# A `>>.&sub` / `>>.&{block}` hyper applies a *callable*, which is never nodal,
# so it descends into nested Iterables down to the leaves (deepmap), unlike a
# nodal method hyper such as `>>.elems`.

plan 13;

# Descend into sublists and apply the block per leaf.
is-deeply (<a b>, <c d e>)».&{ .elems }, ((1, 1), (1, 1, 1)),
    'block hyper descends to leaves (.elems per Str)';

# Nodal method hyper stays at the node level for comparison.
is-deeply (<a b>, <c d e>)».elems, (2, 3),
    'nodal >>.elems applies at node level';

# Deeper nesting is preserved.
is-deeply ((1, 2), (3, (4, 5)))».&{ $_ * 10 }, ((10, 20), (30, (40, 50))),
    'block hyper preserves nested structure';

# Flat list applies the block to each element.
is-deeply (1, 2, 3)».&{ $_ * 10 }, (10, 20, 30),
    'block hyper over a flat list';

# Itemized sublists are descended into as well.
is-deeply ($(1, 2), $(3, 4))».&{ .elems }, ((1, 1), (1, 1)),
    'block hyper descends into itemized sublists';

# Named sub form.
sub twice($x) { $x * 2 }
is-deeply (1, 2, 3)».&twice, (2, 4, 6), 'named-sub hyper over a flat list';
is-deeply ((1, 2), (3,))».&twice, ((2, 4), (6,)), 'named-sub hyper descends';

# Hash target: apply to each value, preserving keys, yielding a Hash.
my %h = a => 1, b => 2;
is-deeply %h».&{ $_ + 10 }, %(a => 11, b => 12),
    'block hyper over a Hash preserves keys';

my %g = a => (1, 2), b => 3;
is-deeply %g».&{ .elems }, %(a => (1, 1), b => 1),
    'block hyper over a Hash descends into values';

# A *nodal* built-in routine (`&elems`, `&reverse`, ...) applied via `>>.&` is
# nodal like `>>.elems`: it stays at the node level and does NOT descend, even
# though a block/user-sub of the same shape would.
my @b := <a b>, <c d e>;
is-deeply @b».&elems, (2, 3), 'nodal-builtin >>.&elems stays at node level';
is-deeply @b».&{ .elems }, ((1, 1), (1, 1, 1)),
    'a block of the same shape still descends';
is-deeply @b».+&elems, ((2,), (3,)),
    'nodal-builtin >>.+&elems is node-level and single-candidate';

# A user sub named after a nodal built-in (`sub elems`) is treated as nodal too,
# mirroring `sub elems is nodal` in roast/S03-metaops/hyper.t: stays at the node
# level. (A plain non-nodal-named sub keeps descending — see the `twice` case.)
sub elems($thing) { $thing.elems }
is-deeply @b».&elems, (2, 3), 'nodal-named user sub stays at node level';
