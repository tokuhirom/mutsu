use Test;

# The cross (X) and zip (Z) metaoperators return a Seq, not a List (matches
# Rakudo). Their inner tuples remain plain Lists.
plan 14;

# Cross.
is (^3 X ^2).^name, 'Seq', 'X returns a Seq';
is (^3 X ^2).raku,
    '((0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)).Seq',
    'X.raku shows .Seq with List inner tuples';
is (1, 2 X 3, 4).^name, 'Seq', 'X with comma operands is a Seq';
is (^2 X ^2 X ^2).^name, 'Seq', 'n-ary X is a Seq';
is (() X ()).^name, 'Seq', 'empty X is a Seq';
is ((1, 2, 3) X (4, 5)).elems, 6, 'X cardinality is preserved';

# Zip.
is (1, 2 Z 3, 4).^name, 'Seq', 'Z returns a Seq';
is (1, 2 Z 3, 4).raku, '((1, 3), (2, 4)).Seq', 'Z.raku shows .Seq';
is (() Z ()).^name, 'Seq', 'empty Z is a Seq';
is (1, 2, 3 Z+ 10, 20, 30).raku, '(11, 22, 33).Seq', 'Z+ reduction is a Seq';
is (<a b> Z=> <1 2>).^name, 'Seq', 'Z=> is a Seq';

# Assigning to an @-array still reifies as Array.
my @r = ^3 X ^2;
is @r.^name, 'Array', 'X assigned to @ becomes an Array';
is @r.elems, 6, 'reified cross has all tuples';

# A lazy operand still works.
is (1 .. * Z "a" .. "c").raku,
    '((1, "a"), (2, "b"), (3, "c")).Seq',
    'Z with a lazy side truncates to the finite side';
