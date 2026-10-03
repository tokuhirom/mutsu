use Test;

# The repeat count of `xx` / `x` is numified like any numeric operand: a Set or
# Hash counts its elements, a Bag its total weight, a Bool 0/1, a Range its
# size. Found in Data::Reshapers: `$missing-value xx $allKeys` with a Set.

plan 7;

is ('W' xx Set(<a b c>)).elems, 3, 'xx Set';
is (1 xx bag(<a a b>)).elems, 3, 'xx Bag counts the total weight';
is (3 xx {a => 1, b => 2}).elems, 2, 'xx Hash';
is (1 xx True).elems, 1, 'xx True';
is (1 xx False).elems, 0, 'xx False';
is (2 xx (1..3)).elems, 3, 'xx Range';
is 'x' x set(<a b>), 'xx', 'x Set';
