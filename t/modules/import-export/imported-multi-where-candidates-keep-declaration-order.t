use Test;
use lib $*PROGRAM.parent(3).add('lib-multi-where-order');

# From the Math::SpecialFunctions distribution (`factorial(0)`): when several
# imported `where` candidates match equally narrowly, the one declared first
# wins, as it does in the declaring module.
plan 7;

use WhereOrder;

is tier(0), 'low', 'the first declared candidate wins over a later even-where';
is tier(1), 'low', 'it also wins over a later odd-where';
is tier(2), 'even', 'an even number reaches the even candidate';
is tier(3), 'odd', 'an odd number above 1 reaches the odd candidate';
is size(0), 'small', 'first declared of two overlapping candidates (no proto)';
is size(1), 'small', 'including at the boundary';
is size(3), 'medium', 'the later candidate still serves its own range';
