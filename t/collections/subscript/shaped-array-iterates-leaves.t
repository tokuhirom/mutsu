use Test;

# A multi-dimensional shaped Array iterates its leaves in row-major order,
# and its `.list` is the array itself (`Array.list` is `self`). mutsu
# iterated the first-dimension rows, and its `.list` was a flat List; a `for`
# loop that wrote its topic back stored a leaf over a row (#14248). Expected
# values are the `raku` (v2026.09) output.

plan 22;

my @c[2;2] = [1,2],[3,4];
my @seen;
@seen.push($_) for @c;
is @seen, [1, 2, 3, 4], 'for iterates the leaves';
is @c.list.elems, 2, '.list.elems counts the first dimension';
isa-ok @c.list, Array, '.list is an Array';
ok @c.list === @c, '.list is the array itself';
is @c.map(* * 10), (10, 20, 30, 40), 'map';
is @c.grep(* > 1), (2, 3, 4), 'grep';
is ([+] @c), 10, 'a reduction';
is @c.join(','), '1,2,3,4', 'join';
is @c.Seq, (1, 2, 3, 4), '.Seq';
is (|@c).elems, 4, 'a slip';
is @c.sum, 10, '.sum';
is @c.max, 4, '.max';
is @c.unique, (1, 2, 3, 4), '.unique';
is @c.first(* > 2), 3, '.first';
is @c.Bag.elems, 4, '.Bag';

for @c { $_ *= 2 }
is @c.raku, 'Array.new(:shape(2, 2), [2, 4], [6, 8])', 'a topic write lands on its leaf';
for @c -> $x is rw { $x++ }
is @c.raku, 'Array.new(:shape(2, 2), [3, 5], [7, 9])', 'an rw parameter write lands on its leaf';
is @c.shape, (2, 2), '... and the shape is kept';

my Int @t[2;3];
@t[1;2] = 5;
is @t.map(*.raku).join(','), 'Int,Int,Int,Int,Int,5', 'a hole iterates as the type object';

my @e[3] = 1, 2, 3;
ok @e.list === @e, 'a one-dimensional shaped array lists as itself';
my int @n[2;2];
isa-ok @n.list, List, 'a native shaped array lists as a List of its leaves';
is @n.list.elems, 4, '... with every leaf';
