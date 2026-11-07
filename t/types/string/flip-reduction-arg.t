use Test;

# From Math::Sequences (Numberphile lunar-add): `+ flip [~] ...`. After
# whitespace a `[` opens a reduction or an array literal for a named unary
# builtin, so `flip [~] 1, 2` is `flip([~] 1, 2)`, not `flip ~ ...`.

plan 8;

is (flip [~] 1, 2), '21', 'flip with a reduction argument';
is (uc [~] "a", "b"), 'AB', 'uc with a reduction argument';
is (+ flip [~] 1, 2), 21, 'prefix + around flip [~]';
is (flip ["ab"]), 'ba', 'flip with an array literal';
is (chars [~] "ab", "c"), 3, 'chars with a reduction';
is (ord [~] "a", "b"), 97, 'ord with a reduction';
my @nums = 12, 345;
is (+ flip [~] (roundrobin @nums.map({.flip.comb})).map: {.max}), 345, 'the lunar-add shape';
is (lc [~] <A B>), 'ab', 'lc with a reduction over a word list';
