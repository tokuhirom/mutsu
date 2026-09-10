use Test;

plan 8;

# The sequence operator inside a subscript takes the WHOLE preceding comma list
# as its seed: `@a[0, 2 ... *]` is `@a[(0,2)...*]`, indices 0,2,4,…, not
# `@a[0, (2...*)]`.
my @array = 3, 7, 9, 11;

is-deeply @array[0, 2 ... *], (3, 9),  'even-index infinite slice seeds from (0,2)';
is-deeply @array[1, 3 ... *], (7, 11), 'odd-index infinite slice seeds from (1,3)';
is-deeply @array[0 ... *],    (3, 7, 9, 11), 'single-seed infinite slice';
is-deeply @array[0, 1 ... *], (3, 7, 9, 11), 'consecutive infinite slice';

# Finite sequence subscripts seed the same way.
is-deeply @array[0, 2 ... 4], (3, 9, Any), 'finite even sequence seeds from (0,2)';

# A plain comma slice (no sequence operator) is unaffected.
is-deeply @array[0, 1, 2], (3, 7, 9), 'plain comma slice still a slice';
is-deeply @array[0 .. 2],  (3, 7, 9), 'range slice still works';

# Three-element seed.
is-deeply @array[0, 1, 2 ... *], (3, 7, 9, 11), 'three-element seed';
