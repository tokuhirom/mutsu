use Test;

plan 2;

# Math::NumberTheory 0.1.4's digit-count uses this object-hash-producing
# classify followed by a hyper method. The result must expose logical keys,
# not their internal .WHICH strings such as Str|1.
my %counts = '123'.comb.classify(*)».elems;
is-deeply %counts, %('1' => 1, '2' => 1, '3' => 1),
    'hyper method preserves object-hash keys';
is %counts.keys».Str.sort.join(','), '1,2,3',
    'hyper method result has the original logical keys';
