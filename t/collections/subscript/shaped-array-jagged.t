use Test;

# Jagged arrays (#19282): a shape that leaves a dimension's length unset, as
# rakudo 2026.11's `Array::Jagged` makes it.

plan 35;

# A first dimension of set length: its rows are made up front.
my @a[2;*];
is-deeply @a.shape, (2, *), '.shape keeps the * dimension';
is @a.elems, 2, 'the rows exist';
is @a.raku, 'Array.new(:shape(2, *), ([], []))', '.raku';
is @a.gist, '[[] []]', '.gist';
@a[0;2] = 5;
is @a.gist, '[[(Any) (Any) 5] []]', 'a row grows to the index written';
is @a[0].elems, 3, 'the row has that many elements';
@a[1] = (1, 2, 3, 4);
is @a.gist, '[[(Any) (Any) 5] [1 2 3 4]]', 'a stored row takes the items';
is @a[1;3], 4, 'a stored row reads back';
@a[1].push(9);
is @a[1].gist, '[1 2 3 4 9]', 'a * row is a growable Array';
throws-like { @a.push([1]) }, X::IllegalOnFixedDimensionArray, 'push on the set dimension';
throws-like { @a.pop }, X::IllegalOnFixedDimensionArray, 'pop on the set dimension';
throws-like { @a[5;0] = 1 }, Exception, message => 'Index 5 for dimension 1 out of range (must be 0..1)',
    'an index past the set dimension';
throws-like { @a[3] = 1 }, Exception, message => 'Index 3 for dimension 1 out of range (must be 0..1)',
    'a row stored past the set dimension';

# Initialization, iteration and the whole-array assignment.
my @b[2;*] = (1, 2), (3, 4, 5);
is @b.gist, '[[1 2] [3 4 5]]', 'initialized from rows';
is-deeply @b.map(*.elems).List, (2, 3), 'iterating gives the rows';
is-deeply @b[*;0].List, (1, 3), 'a Whatever over the rows';
@b = (7, 8, 9), (1,);
is @b.gist, '[[7 8 9] [1]]', 'a whole assignment takes the rows';
is-deeply @b.shape, (2, *), '... and keeps the shape';
my @copy = @b;
is-deeply @copy.shape, (*,), 'a copy is a plain Array';
ok (my @c[2;*]) eqv (my @d[2;*]), 'two jagged arrays of one shape are eqv';
nok [[], []] eqv (my @e[2;*]), 'a plain Array of Arrays is not';

# A * first dimension: rows are made of the row shape as they are written.
my @f[*;3];
is-deeply @f.shape, (*, 3), '.shape of [*;3]';
is @f.elems, 0, 'no rows yet';
@f[1;2] = 7;
is @f.gist, '[[(Any) (Any) (Any)] [(Any) (Any) 7]]', 'the rows before it are made too';
is-deeply @f[0].shape, (3,), 'each row is a shaped array';
throws-like { @f[0;5] = 1 }, Exception, message => 'Index 5 for dimension 2 out of range (must be 0..2)',
    'an index past a row dimension';
@f[3] = (1, 2);
is @f[3].gist, '[1 2 (Any)]', 'a stored row of a shaped row shape';
is @f.raku, 'Array.new(:shape(*, 3), (Array.new(:shape(3,), [Any, Any, Any]), '
    ~ 'Array.new(:shape(3,), [Any, Any, 7]), Array.new(:shape(3,), [Any, Any, Any]), '
    ~ 'Array.new(:shape(3,), [1, 2, Any])))', '.raku of shaped rows';

# More dimensions.
my @g[2;3;*];
@g[1;2;4] = 'x';
is @g.gist, '[[[] [] []] [[] [] [(Any) (Any) (Any) (Any) x]]]', 'three dimensions';
is-deeply @g[1].shape, (3, *), 'a row of a jagged row shape is jagged';
my @h[*;*];
@h[2;1] = 3;
is @h.gist, '[[] [] [(Any) 3]]', 'every dimension unset';

# A typed jagged array.
my Int @i[2;*];
@i[0;1] = 4;
is @i.gist, '[[(Int) 4] []]', 'the slots before a write hold the type object';
is @i.of, Int, '.of';
throws-like { @i[0;0] = "x" }, X::TypeCheck::Assignment, 'a value of another type';

is Array.new(:shape(2, *), [1], [2, 3]).gist, '[[1] [2 3]]', 'Array.new with rows';
