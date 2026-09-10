use Test;

# X and Z are list-associative: `a X b X c` is a single n-ary cross/zip
# producing flat n-tuples, not left-nested pairs.

plan 22;

# --- Cross (X) chained: flat tuples ---
is (1 X 2 X 3).raku, '((1, 2, 3),).Seq', 'scalar X chain builds one 3-tuple';
is (1 X 2 X 3 X 4).raku, '((1, 2, 3, 4),).Seq', 'scalar X chain builds one 4-tuple';
is ((1,2) X (3,4) X (5,6)).elems, 8, 'X of three 2-lists has 8 combos';
is ((1,2) X (3,4) X (5,6))[0].raku, '(1, 3, 5)', 'first X combo is a flat triple';
is ((1,2) X (3,4) X (5,6))[7].raku, '(2, 4, 6)', 'last X combo is a flat triple';

# --- Cross with operator: n-ary reduction ---
is ((1,2) X+ (10,20) X+ (100,200)).List, (111, 211, 121, 221, 112, 212, 122, 222), 'X+ chain sums across all operands';
is (<a b> X~ <c d> X~ <e f>).List, <ace acf ade adf bce bcf bde bdf>, 'X~ chain concatenates across all operands';
is ((1..2) X* (1..2) X* (1..2)).List, (1, 2, 2, 4, 2, 4, 4, 8), 'X* chain multiplies across all operands';

# --- Zip (Z) chained: flat tuples ---
is (1 Z 2 Z 3).raku, '((1, 2, 3),).Seq', 'scalar Z chain builds one 3-tuple';
is ((1,2) Z (3,4) Z (5,6)).elems, 2, 'Z of three 2-lists has 2 tuples';
is ((1,2) Z (3,4) Z (5,6))[0].raku, '(1, 3, 5)', 'first Z tuple is a flat triple';
is ((1,2) Z (3,4) Z (5,6))[1].raku, '(2, 4, 6)', 'second Z tuple is a flat triple';

# --- Zip with operator: n-ary reduction ---
is ((1,2) Z+ (10,20) Z+ (100,200)).List, (111, 222), 'Z+ chain sums positionally across all operands';
is ((1,2,3) Z~ <a b c> Z~ <x y z>).List, <1ax 2by 3cz>, 'Z~ chain concatenates positionally';
is ((1,2,3) Z* (4,5,6) Z* (7,8,9)).List, (28, 80, 162), 'Z* chain multiplies positionally';

# --- Zip stops at the shortest operand ---
is ((1,2,3) Z (4,5) Z (6,7,8,9)).elems, 2, 'Z chain stops at the shortest operand';

# --- Ranges as operands ---
is (1..3 Z 4..6 Z 7..9).List, ((1,4,7), (2,5,8), (3,6,9)), 'Z chain over ranges';
is ((1..2) X (1..2) X (1..2)).elems, 8, 'X chain over ranges has 8 combos';

# --- 2-operand cases unchanged ---
is (1 X 2).raku, '((1, 2),).Seq', '2-operand X still builds a pair';
is (1 Z 2).raku, '((1, 2),).Seq', '2-operand Z still builds a pair';

# --- Empty operand short-circuits the whole cross ---
is ((1,2) X () X (3,4)).elems, 0, 'X chain with an empty operand is empty';

# --- Z=> still pairs up ---
is ((1,2,3) Z=> <a b c>).map(*.value).List, <a b c>, 'Z=> pairs each key with its value';
