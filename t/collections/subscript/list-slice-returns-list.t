use Test;

# A positional slice of any positional array — a List `(1,2,3)`, an Array
# `[1,2,3]`, or an itemized `$(...)`/`$[...]` — decontainerizes to a List in
# Raku, NOT a Seq. Previously mutsu returned a Seq for List-literal slices.

plan 14;

# List literal
is (10,20,30)[1,2].^name, 'List', 'List slice is a List';
is-deeply (10,20,30)[1,2], (20,30),  'List slice values';
is (10,20,30)[1..2].^name, 'List',   'List range-slice is a List';
is <a b c d>[1,3].^name,   'List',   'word-list slice is a List';
is-deeply <a b c d>[1,3], ('b','d'), 'word-list slice values';

# Array
is [1,2,3][1,2].^name,     'List',   'Array slice is a List';
is-deeply [1,2,3][1,2], (2,3),       'Array slice values';

# Itemized
is $(1,2,3)[0,2].^name,    'List',   'itemized-list slice is a List';
is $[1,2,3][0,2].^name,    'List',   'itemized-array slice is a List';

# Bound array variable
{
    my @a = 10,20,30;
    is @a[1,2].^name, 'List', 'bound-array slice is a List';
    is-deeply @a[1,2], (20,30), 'bound-array slice values';
}

# A single (non-slice) index is NOT wrapped in a List.
is (10,20,30)[1].^name, 'Int', 'single index is the element, not a List';

# grep/map still produce a Seq (unchanged).
is (1,2,3,4).grep(* > 2).^name, 'Seq', 'grep still returns a Seq';
is (1,2,3).map(* * 2).^name,    'Seq', 'map still returns a Seq';
