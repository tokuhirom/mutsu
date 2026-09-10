use Test;

plan 5;

my @pairs[1; 2];
for @pairs.pairs -> $pair {
    $pair.value = $pair.key[1] + 40;
}
is-deeply @pairs[0; 0], 40, 'multidimensional .pairs value writes through';
is-deeply @pairs[0; 1], 41, 'tuple pair key selects the correct leaf';

my int @native[2; 3] Z= 0..5;
is +@native, 2, 'native shaped Z= preserves the outer dimension';
is @native[0; 1], 1, 'native shaped Z= fills the first row';
is @native[1; 2], 5, 'native shaped Z= fills the last row';
