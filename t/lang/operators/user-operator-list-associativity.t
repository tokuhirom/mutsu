use Test;

# A user infix declared `is assoc<list>` takes the whole operand list in one
# call, both chained (`a op b op c`) and reduced (`[op] a, b, c`), and an
# `is equiv(...)` written after `is assoc<list>` keeps it list-associative.
# Found in OneSeq: `my sub infix:«>>>»(**@iterables) is assoc<list> is equiv(&[~])`.

plan 8;

my sub infix:«>>>»(**@its) is assoc<list> is equiv(&[~]) { @its.elems }
is 1 >>> 2 >>> 3, 3, 'chain with assoc then equiv is one call';
is ([>>>] 1, 2, 3), 3, 'reduction is one call';
is ([>>>] 1), 1, 'reduction of one element calls with one argument';
is ([>>>] ()), 0, 'reduction of nothing calls with no arguments';

my sub infix:<+++>(**@its) is equiv(&[~]) is assoc<list> { @its.join('|') }
is 1 +++ 2 +++ 3, '1|2|3', 'chain with equiv then assoc';
is ([+++] 4, 5, 6), '4|5|6', 'its reduction';

my sub infix:<mul>($a, $b) is equiv(&[*]) { $a * $b }
is 2 mul 3 mul 4, 24, 'a binary equiv operator still folds left';
is ([mul] 2, 3, 4), 24, 'and reduces pairwise';
