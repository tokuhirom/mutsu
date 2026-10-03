use Test;

# A chain of four or more `Z` operands is one n-ary zip of flat tuples, inside
# parentheses too. Found in Data::Reshapers' t/14:
# `(11 .. 13 Z <a|b bIc x|y> Z <a b x> Z <b c y>)`.

plan 5;

is (1 Z 2 Z 3 Z 4).raku, '((1, 2, 3, 4),).Seq', 'four scalars';
is-deeply (11 .. 12 Z <a b> Z <c d> Z <e f>).List,
    ((11, 'a', 'c', 'e'), (12, 'b', 'd', 'f')), 'four lists';
is-deeply (1 Z 2 Z 3 Z 4 Z 5).List, ((1, 2, 3, 4, 5),), 'five operands';
is-deeply ((1, 2) Z+ (3, 4) Z+ (5, 6) Z+ (7, 8)).List, (16, 20), 'Z+ chain';
is-deeply (1 Z 2 Z 3).List, ((1, 2, 3),), 'three operands unchanged';
