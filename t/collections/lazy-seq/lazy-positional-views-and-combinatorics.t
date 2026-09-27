use Test;

# An Array's .keys / .values / .kv / .pairs / .antipairs / .batch and every
# list's .combinations / .permutations are lazy Seqs (#9158): a consumer that
# stops early pulls only what it needs, and the Array views read the live
# array, as Rakudo's iterators do.

plan 24;

my @a = ^10;
is @a.keys.head(3), (0, 1, 2), '.keys.head';
is @a.values.head(2), (0, 1), '.values.head';
is @a.kv.head(5), (0, 0, 1, 1, 2), '.kv.head stops between a key and its value';
is @a.pairs.head(2).map(*.raku).join(' '), '0 => 0 1 => 1', '.pairs.head';
is @a.antipairs.head(2).map(*.raku).join(' '), '0 => 0 1 => 1', '.antipairs.head';
is @a.batch(4).head.raku, '(0, 1, 2, 3)', '.batch.head';

# Views read the live array.
{
    my @b = 1, 2, 3;
    my $k = @b.keys;
    @b.push(4);
    is $k, (0, 1, 2), '.keys counts the elements there were at the call';
    my @c = 1, 2, 3;
    my $p = @c.pairs;
    @c[0] = 9;
    is $p.head.value, 9, '.pairs sees a later store';
    my @d = 1 .. 5;
    my $b = @d.batch(2);
    @d.push(6);
    is $b, ((1, 2), (3, 4), (5, 6)), '.batch sees a later push';
}

# Element containers still alias the array.
{
    my @b = ^5;
    for @b.kv -> $i, $v is rw { $v = $i * 2 if $i < 3 }
    is @b, [0, 2, 4, 3, 4], '.kv hands out rw element containers';
    for @b.pairs { .value = 0 if .key == 4 }
    is @b, [0, 2, 4, 3, 0], '.pairs hands out rw element containers';
    $_++ for @b.values;
    is @b, [1, 3, 5, 4, 1], '.values hands out rw element containers';
}

# Combinatorics, in Rakudo's order.
is (^4).combinations(2), ((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)), 'combinations(2)';
is (^3).combinations, ((), (0,), (1,), (2,), (0, 1), (0, 2), (1, 2), (0, 1, 2)), 'combinations';
is (^4).combinations(1..2).elems, 10, 'combinations(Range)';
is (^2).combinations(3).elems, 0, 'combinations(k > elems) is empty';
is (^3).permutations, ((0, 1, 2), (0, 2, 1), (1, 0, 2), (1, 2, 0), (2, 0, 1), (2, 1, 0)), 'permutations';
is combinations(3).elems, 8, 'combinations(Int) sub';
is permutations(3)[5], (2, 1, 0), 'permutations(Int) sub';

# A consumer that stops early does not build the rest.
is (^1000).combinations(2).head(2), ((0, 1), (0, 2)), 'combinations(2).head on a big list';
is (^12).permutations.head, (^12).List, 'permutations.head does not build 12! permutations';
is (^12).permutations.head(2)[1], (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10), 'permutations.head(2)';

my $s = (^3).permutations;
is $s.elems, 6, 'a stored permutations Seq counts';
is $s[1], (0, 2, 1), '... and indexes after counting';
