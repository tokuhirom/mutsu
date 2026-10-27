use Test;

# `a X b X c` is one n-ary product (flat tuples), but a parenthesized operand
# is one operand: `(a X b) X c` keeps its inner tuples nested. (Found with
# Red's `t/91-cross-product`: "Parentheses retain nested tuples".)

plan 10;

is ((1, 2) X ^2 X <a b>).Array.raku, '[(1, 0, "a"), (1, 0, "b"), (1, 1, "a"), (1, 1, "b"), (2, 0, "a"), (2, 0, "b"), (2, 1, "a"), (2, 1, "b")]',
    'unparenthesized: flat triples';
is (((1, 2) X ^2) X <a b>).Array.raku, '[((1, 0), "a"), ((1, 0), "b"), ((1, 1), "a"), ((1, 1), "b"), ((2, 0), "a"), ((2, 0), "b"), ((2, 1), "a"), ((2, 1), "b")]',
    'a parenthesized left product stays one operand';
is ((<a b> X (1, 2)) X <x>).head.raku, '(("a", 1), "x")', 'nested pair in a triple';
is (<a b> X ((1, 2) X <x>)).head.raku, '("a", (1, "x"))', 'a parenthesized right product stays nested';
is (<a b> X (1, 2) X <x>).head.raku, '("a", 1, "x")', 'chained: flat';

is ((1, 2) Z (3, 4) Z (5, 6)).List.raku, '((1, 3, 5), (2, 4, 6))', 'Z chain: flat triples';
is (((1, 2) Z (3, 4)) Z (5, 6)).List.raku, '(((1, 3), 5), ((2, 4), 6))', 'a parenthesized Z stays one operand';

is (((1, 2) X+ (10, 20)) X+ (100, 200)).List.raku, '(111, 211, 121, 221, 112, 212, 122, 222)', 'X+ over a parenthesized X+ sums';
is ((1, 2) X+ (10, 20) X+ (100, 200)).List.raku, '(111, 211, 121, 221, 112, 212, 122, 222)', 'X+ chain sums';
is (((1 X 2) X 3)).raku, '(((1, 2), 3),).Seq', 'scalar nesting';

done-testing;
