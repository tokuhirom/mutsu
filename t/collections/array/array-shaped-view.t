use Test;

# A row of a multi-dimensional shaped array is an `Array::ShapedView`: a
# fixed-shape view whose reads and writes go through to the parent (#19282).

plan 19;

my @a[2;3] = (1,2,3),(4,5,6);
my $r = @a[1];

is $r.WHAT.^name, 'Array::ShapedView', 'a row of a 2-dim shaped array is a ShapedView';
is $r.raku, 'Array.new(:shape(3,), [4, 5, 6])', '.raku of a row view';
is $r.elems, 3, '.elems of a row view';
is-deeply $r.shape, (3,), '.shape of a row view';
is $r[2], 6, 'reading through a row view';

$r[0] = 40;
is @a[1;0], 40, 'a write through a row view reaches the parent';
@a[1;1] = 50;
is $r[1], 50, 'a write to the parent is seen through the row view';

is-deeply $r.List, (40, 50, 6), '.List of a row view';
my @copy := $r.Array;
is @copy.raku, 'Array.new(:shape(3,), [40, 50, 6])', '.Array of a row view keeps its shape';
is @copy.WHAT.^name, 'Array', '.Array of a row view is a plain shaped Array';
@copy[0] = 0;
is @a[1;0], 40, '.Array of a row view is a copy';

throws-like { $r.push(1) }, X::IllegalOnFixedDimensionArray,
    'a row view refuses to grow';

my @seen;
for @a[0] -> $x { @seen.push($x) }
is-deeply @seen, [1, 2, 3], 'for over a row view iterates its elements';
is-deeply @a[0].map(* * 2).List, (2, 4, 6), '.map over a row view';
ok @a[1] eqv @a[1], 'a row view is eqv to itself';
is @a[1].sum, 96, '.sum of a row view';

my @b[2;2;2];
@b[1;0;1] = 'q';
is @b[1].WHAT.^name, 'Array::ShapedView', 'a 3-dim row is a ShapedView';
is-deeply @b[1].shape, (2, 2), 'a 3-dim row has the remaining dimensions';
is @b[1][0][1], 'q', 'nested row views read the parent';
