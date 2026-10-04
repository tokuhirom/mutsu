use Test;

# Found via the ML::SparseMatrixRecommender suite (t/01-creation):
# `<a b>.map(-> $t { @rows.map({ ... }) }).flat(1)` must descend into the
# Seq elements one level.
plan 6;

my @rows = (1, 2, 3);
my $long = <a b>.map(-> $t { @rows.map({ %(id => $_, v => $t) }) }).flat(1);
is $long.elems, 6, 'flat(1) flattens a list of Seqs';
is $long.all ~~ Map:D, True, 'the elements are the maps';

is ((1, 2).Seq, (3, 4).Seq).flat(1).elems, 4, 'Seq elements of a List, depth 1';
is ((1, 2).map({ $_ }), 3).flat(0).elems, 2, 'depth 0 leaves the Seq alone';
is (1, (2, (3, 4)).Seq).flat(1).raku, '(1, 2, (3, 4)).Seq', 'depth 1 stops after one level';
is ((1, (2, 3).Seq).Seq,).flat(1).raku, '(1, (2, 3).Seq).Seq', 'nested Seq under depth 1';
