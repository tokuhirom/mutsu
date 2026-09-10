use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

is-deeply-junction any(1, 2, 3), any(1, 2, 3), 'any junction with same values matches';

# Order should not matter for junction guts comparison.
is-deeply-junction any(1, 2, 3), any(3, 2, 1), 'any junction matches regardless of value order';

is-deeply-junction all("a", "b"), all("a", "b"), 'all junction with strings matches';

is-deeply-junction any(all(1, 2), 3), any(3, all(2, 1)), 'nested junction structure matches';
