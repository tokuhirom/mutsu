use Test;

plan 22;

# #13494: .^methods lists the methods Rakudo declares directly on Pair, the
# Setty/Baggy/Mixy classes, Map, List, Seq and Array, not only the ones a
# composed role brings.
sub names(Mu \t) { t.^methods.map(*.name).Set }

for (Pair, <freeze iterator new>),
    (Set, <iterator new deepmap minpairs maxpairs>),
    (SetHash, <iterator new deepmap minpairs maxpairs>),
    (Bag, <iterator new deepmap>),
    (Mix, <iterator new deepmap>),
    (Map, <iterator new sort>),
    (List, <iterator new>),
    (Seq, <iterator new sort eager rotate>),
    (Array, <iterator new>) -> ($t, @want) {
    my $have = names($t);
    ok @want.all ∈ $have, "{$t.^name}.^methods lists @want[]";
}

# What is listed is callable.
is Pair.new(1, 2).raku, '1 => 2', 'Pair.new';
is (a => 1).freeze.raku, '1', 'Pair.freeze';
is (a => 1).iterator.pull-one.raku, ':a(1)', 'Pair.iterator';
is set(1, 2).minpairs.elems, 2, 'Set.minpairs';
is Map.new((a => 1)).sort.elems, 1, 'Map.sort';
is (1, 2, 3).Seq.rotate(1).List, (2, 3, 1), 'Seq.rotate';
is bag(1, 1).deepmap(*.so).elems, 1, 'Bag.deepmap';
ok Pair.^can('iterator').elems >= 1, 'Pair.^can finds iterator';
ok Pair.^methods.elems >= 25, 'Pair.^methods grew from 22';
ok Map.^methods.elems >= 38, 'Map.^methods grew from 37';
is Set.^methods.map(*.name).grep('iterator').elems, 1, 'no duplicate entries';
is Array.^methods.map(*.name).grep('new').elems, 1, 'Array.new listed once';
is Seq.^methods.map(*.name).grep('eager').elems, 1, 'Seq.eager listed once';
