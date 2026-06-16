use Test;

# A single-element array literal `[x]` flattens `x` one level when `x` is a
# Positional/list-like value (List, Array, Seq, Range) or a Hash, but a
# Set/Bag/Mix is NOT Positional and must stay whole as one element.
# Previously `[set(1,2)]` decomposed the Set into its `key => True` pairs
# (elems 2) because the single-element flatten path used value_to_list, which
# expands a Set to pairs. Matches raku and `flat(set(...))`.

plan 16;

# --- Set/Bag/Mix stay whole (one element) ---
is [set(1, 2)].elems, 1, '[set] keeps the Set whole';
is [set(1, 2)][0].WHAT.^name, 'Set', '[set][0] is a Set';
is [bag(1, 1, 2)].elems, 1, '[bag] keeps the Bag whole';
is [bag(1, 1, 2)][0].WHAT.^name, 'Bag', '[bag][0] is a Bag';
is [(a => 1).Mix].elems, 1, '[Mix] keeps the Mix whole';
is [(a => 1).Mix][0].WHAT.^name, 'Mix', '[Mix][0] is a Mix';

# A parenthesised single-Set list behaves the same.
is [(set(1, 2),)].elems, 1, '[(set,)] keeps the Set whole';

# --- list-like values still flatten one level ---
is [(1, 2, 3)].elems, 3, '[(1,2,3)] flattens the List';
is [1 .. 3].elems, 3, '[1..3] flattens the Range';
is [<a b c>].elems, 3, '[<a b c>] flattens the word list';
my @nums = 1, 2, 3, 4;
is [@nums].elems, 4, '[@arr] flattens the array';

# --- Hash still flattens to its pairs (Hash IS flattened, unlike Set) ---
is [{ a => 1, b => 2 }].elems, 2, '[{...}] flattens the Hash to pairs';
is [%(a => 1, b => 2, c => 3)].elems, 3, '[%h] flattens the Hash to pairs';

# --- nested arrays / itemized values keep their own count ---
is [[1, 2], [3, 4]].elems, 2, '[[..],[..]] keeps each nested array';
is [$(1, 2, 3)].elems, 1, '[$(...)] keeps the itemized list whole';

# --- multi-element form: each Set is one element ---
is [set(1, 2), bag(3), 9].elems, 3, 'multi-element [set, bag, 9] keeps each whole';
