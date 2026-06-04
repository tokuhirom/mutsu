use Test;

# Set/Bag/Mix coercion uses list-context flattening: a List `(...)` invocant
# flattens its elements (recursively, Lists are transparent), but an Array
# `[...]` (or itemized) invocant takes each element whole. A nested Array is
# therefore a single element, but a nested List flattens. Mirrors the rules
# exercised by roast/S02-types/{set,bag,mix}.t and the nodal `».Set`/`».Bag`/
# `».Mix` subtests in roast/S03-metaops/hyper.t.

plan 14;

# --- Array invocant: elements taken whole (nested array NOT flattened) ---
is [1, [2, 3]].Set.elems, 2, '[1,[2,3]].Set keeps nested array as one element';
is [4, [5, 6]].Bag.elems, 2, '[4,[5,6]].Bag keeps nested array as one key';
is [4, [5, 6]].Mix.elems, 2, '[4,[5,6]].Mix keeps nested array as one key';
is [1, (2, 3)].Set.elems, 2, '[1,(2,3)].Set keeps nested list as one element (array invocant)';
is [1, [2, [3, 4]]].Set.elems, 2, 'array invocant does not flatten at any depth';

# --- List invocant: elements flattened (Lists recursively, Arrays one level) ---
is (1, [2, 3]).Set.elems, 3, '(1,[2,3]).Set flattens nested array';
is ((1, 2), 3).Set.elems, 3, '((1,2),3).Set flattens nested list';
is (1, (2, (3, 4))).Set.elems, 4, 'list invocant flattens nested lists recursively';
my @a = 1, 2, 3;
is (@a, 9).Set.elems, 4, '(@a, 9).Set flattens the @-array';
is [@a, 9].Set.elems, 2, '[@a, 9].Set keeps the @-array whole';

# --- the nested array survives as a real Array key (gist shows brackets) ---
is-deeply [4, [5, 6]].Set.keys.sort, (4, [5, 6]), 'nested array is a Set key, not flattened';
is ([4, [5, 6]].Bag.keys.sort).gist, '(4 [5 6])', 'Bag key gist shows the array brackets';

# --- hyper distributes the coercion per node (Array invocants) ---
is ([[2, 3], [4, [5, 6]]]».Set».keys».sort).gist, '((2 3) (4 [5 6]))',
    '».Set is nodal: each node coerced, nested array kept';
is ([[2, 3], [4, [5, 6]]]».Bag».keys».sort).gist, '((2 3) (4 [5 6]))',
    '».Bag is nodal with nested array preserved';
