use Test;

# A real Array can reach the generic map dispatcher through a value-producing
# expression (`@a.list`) or through an itemized scalar holding `$[...]`. Its elements are
# still writable containers, so `$_` mutations must reach the backing Array at
# consumption time. A List literal remains immutable and is covered by the
# ordinary map topic tests.

plan 10;

my @a = 1, 2, 3;
my $mapped = @a.list.map({ $_ = 7 });
is $mapped.^name, 'Seq', '@a.list.map returns a Seq';
is-deeply @a, [1, 2, 3], '@a.list.map does not write before consumption';
$mapped.eager;
is-deeply @a, [7, 7, 7], '@a.list.map writes through at consumption';

my @b = 1, 2, 3;
@b.list.map({ $_++ }).eager;
is-deeply @b, [2, 3, 4], '@a.list.map preserves increment writeback';

my $array = $[1, 2, 3];
my $scalar_map = $array.map({ $_ = 5 });
is $scalar_map.^name, 'Seq', 'a scalar-held Array map returns a Seq';
is-deeply $array, [1, 2, 3], 'scalar-held Array map is lazy';
$scalar_map.eager;
is-deeply $array, [5, 5, 5], 'scalar-held Array map writes through';

my @c = 1, 2, 3;
@c.list.grep({ $_ = 9 }).eager;
is-deeply @c, [9, 9, 9], 'the existing list grep writeback remains intact';

throws-like {
    (1, 2, 3).map({ $_ = 4 }).eager;
}, X::AdHoc, 'an immutable List map still rejects topic assignment';

my $literal_array = [1, 2, 3];
$literal_array.map({ $_ = 4 }).eager;
is-deeply $literal_array, [4, 4, 4], 'a real Array map may assign to its elements';
