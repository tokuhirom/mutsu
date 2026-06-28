use Test;

# A subscript slice with a top-level comma (`@a[1,2]`) inside a string must
# interpolate ALL the selected elements (not just the first index), and a
# trailing method call must chain. Regression: integration/advent2012-day10.t.

plan 6;

my @beer = <Chimay Hobgoblin Yeti>;
is "[@beer[1,2]]", "[Hobgoblin Yeti]", "array slice interpolates all elements";
is "x @beer[1,2].join(' and ') y", "x Hobgoblin and Yeti y",
   "slice + .join chains in interpolation";
is "[@beer[0]]", "[Chimay]", "single-index interpolation still works";
is "[@beer[0,2]]", "[Chimay Yeti]", "non-adjacent slice indices";

my %h = a => 1, b => 2, c => 3;
is "[%h{'a','c'}]", "[1 3]", "hash slice with top-level commas interpolates all";

# a no-paren method after a subscript is NOT interpolated (matches Rakudo;
# only `.method(...)` with parens chains in interpolation)
is "[@beer[0].uc]", "[Chimay.uc]", "no-paren method after subscript not interpolated";
