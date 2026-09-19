use Test;

# Regression reduced from Data::StaticTable 0.1.1's row-building map callback.
plan 2;

my @seen;
my @values = (1, 2, 3, 4).rotor(2).map: -> ($a, $b) {
    LAST { @seen.push($b) }
    @seen.push($a);
    $a + $b
};

is-deeply @values.List, (3, 7),
    'a map block with a sub-signature keeps its values';
is-deeply @seen.List, (1, 3, 4),
    'LAST runs once with the final sub-signature binding';
