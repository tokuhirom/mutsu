# `<=>` is a numeric operator: an Array/List/Set/…/Range operand is coerced to
# its element count (`.Numeric` = `.elems`) before comparing, NOT compared
# structurally (that is `cmp`). The inline `{ $^a <=> $^b }` / `* <=> *` sort
# fast path must match the standalone operator, so a sort by `<=>` over
# aggregates orders by `.elems`.
use Test;

plan 10;

# Standalone `<=>` over arrays coerces to .elems (regression baseline).
is ([1, 2, 9] <=> [3, 4]), More, 'Array <=> Array coerces to .elems (3 <=> 2)';
is ([1, 2] <=> [3, 4, 5]), Less, 'shorter array is Less';
is ([1, 2, 3] <=> [4, 5, 6]), Same, 'equal-length arrays are Same';
is ([1, 2] <=> 5), Less, 'Array <=> Int coerces the array to .elems';

# The inline `{ $^a <=> $^b }` sort fast path must match — order by .elems.
is-deeply
    ([1, 2, 9], [3, 4]).sort({ $^a <=> $^b }).map(*.elems).List,
    (2, 3),
    'sort({ $^a <=> $^b }) over arrays orders by .elems, not structurally';
is-deeply
    ([1, 2, 9], [3, 4]).sort(* <=> *).map(*.elems).List,
    (2, 3),
    'sort(* <=> *) over arrays orders by .elems';
is-deeply
    ([1, 2, 3], [1], [1, 2]).sort({ $^b <=> $^a }).map(*.elems).List,
    (3, 2, 1),
    'reversed { $^b <=> $^a } over arrays orders by .elems descending';

# `cmp` stays structural (must NOT be affected).
is-deeply
    ([1, 2, 9], [3, 4]).sort({ $^a cmp $^b }).map(*.elems).List,
    (3, 2),
    'sort({ $^a cmp $^b }) stays structural (compares 1 vs 3 first)';

# Numeric scalars keep their exact ordering (Rat precision, not float).
is-deeply
    (1/3, 1/2, 1/4).sort({ $^a <=> $^b }).List,
    (1/4, 1/3, 1/2),
    'Rat sort by <=> stays exact';
is-deeply
    (3, 1, 2).sort({ $^a <=> $^b }).List,
    (1, 2, 3),
    'plain Int sort by <=> unaffected';
