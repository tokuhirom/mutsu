use Test;

# An Array's .rotor / .flat / .tree are lazy Seqs over the live array (#9158):
# a consumer that stops early pulls only what it needs.

plan 15;

my @a = ^10;
is @a.rotor(3), ((0, 1, 2), (3, 4, 5), (6, 7, 8)), 'rotor';
is @a.rotor(3, :partial), ((0, 1, 2), (3, 4, 5), (6, 7, 8), (9,)), 'rotor :partial';
is @a.rotor(2 => -1).head(3), ((0, 1), (1, 2), (2, 3)), 'rotor with overlap, head';
is @a.rotor(1 .. 3), ((0,), (1, 2), (3, 4, 5), (6,), (7, 8)), 'rotor with a Range of counts';
is @a.rotor(0, 1, *).elems, 3, 'rotor with a zero count';

{
    my @b = 1, 2, 3, 4;
    my $r = @b.rotor(2);
    @b.push(5, 6);
    is $r, ((1, 2), (3, 4), (5, 6)), 'rotor reads the array when pulled';
}

my @n = 1, (2, 3), [4, 5];
is @n.flat, (1, (2, 3), [4, 5]), 'an Array keeps its itemized elements';
is (1, (2, 3), [4, 5]).flat, (1, 2, 3, 4, 5), 'a List flattens its elements';
is $[1, [2]].flat.elems, 2, 'an itemized Array is de-itemized first';
{
    my @c = 1, 2;
    @c = @c.flat;
    is @c, [1, 2], 'assigning an array its own flat';
}
is (^1_000_000).List.flat.head(2), (0, 1), 'flat.head on a big List';

is [1, [2, [3]]].tree.raku, '$((1, $((2, $((3,).Seq)).Seq)).Seq)', 'tree';
is (1, (2, (3,))).tree.raku, '$((1, $((2, $((3,).Seq)).Seq)).Seq)', 'tree of a List';
is [1, [2, 3]].tree(0).raku, '[1, [2, 3]]', 'tree(0) is the identity';

my @big = ^200_000;
is @big.tree.list.head(2), (0, 1), 'tree of a big array, pulled a prefix';
