use v6;
use Test;

# List-like receivers numify to their element count before applying a numeric
# method. Grid uses this form when computing the side of a square matrix.
plan 4;

my @items = ^16;
is-approx @items.sqrt, 4, 'Array.sqrt uses the element count';
is-approx @items.keys.sqrt, 4, 'a List.sqrt uses the element count';
is-approx (0, 1, 2, 3).sqrt, 2, 'a parenthesized List.sqrt uses the count';
is @items.sqrt.Int, 4, 'List.sqrt returns a numeric result';

done-testing;
