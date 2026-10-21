use Test;

# A typed multi-dimensional shaped array keeps its shape through the
# declaration's element coercion, so `.list` / `.List` flatten its leaves.
# The coercion rebuilt the array without its shape, and a native one
# (`my int @a[2;2]`) then had no `.list` at all (#14243; PDF's
# `t/io-util.t`). Expected values are the `raku` (v2026.09) output.

plan 7;

my int @b[2;2];
is @b.list.join(','), '0,0,0,0', 'a native 2-dim array lists its leaves';
is @b.list.WHAT.^name, 'List', '... as a List';

my uint64 @in[2;2] = [1, 2], [3, 4];
is @in.list.join(','), '1,2,3,4', 'an initialized native 2-dim array lists its leaves';
is @in.List.join(','), '1,2,3,4', '.List flattens it too';

my int @c[2;2];
@c[1;1] = 7;
is @c.shape.join(','), '2,2', 'the shape survives an element store';
is @c.List.join(','), '0,0,0,7', '... and so does the flattening';

my Int @t[2;2];
@t[0;1] = 5;
is @t.List.map({ .defined ?? $_ !! 'U' }).join(','), 'U,5,U,U',
    'a typed (non-native) 2-dim array flattens with its type-object gaps';
