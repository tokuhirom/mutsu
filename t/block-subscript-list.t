use Test;

plan 14;

# A Block used as a subscript is invoked; when it returns a list of indices
# (e.g. `{0,1}` returns the List `(0,1)`), every element is used as a slice
# index. A scalar or Range return keeps its existing meaning.

my @a = 10, 20, 30, 40, 50;

# --- single-dimension array receiver ---
is-deeply @a[{0,1}],   (10, 20),      'block returning a list slices the array';
is-deeply @a[{ 2 }],   30,            'block returning a scalar is a single index';
is-deeply @a[{ 1..3 }], (20, 30, 40), 'block returning a Range slices the array';
is-deeply @a[-> { 0, 4 }], (10, 50),  'pointy block returning a list slices the array';
is-deeply @a[{ @a.elems - 1 }], 50,   'elems-relative block index still works';

# WhateverCode and plain slices are unaffected
is-deeply @a[*-1],        50,          'WhateverCode last index unchanged';
is-deeply @a[*-2 .. *-1], (40, 50),    'WhateverCode range slice unchanged';
is-deeply @a[0, 2, 4],    (10, 30, 50),'plain list slice unchanged';

# --- nested / multi-dimensional array receiver ---
my @m = [1, 2, 3], [4, 5, 6], [7, 8, 9];
is-deeply @m[1][{0, 2}],   (4, 6),                'block-list subscript on a nested array';
is-deeply @m[0; {0,1}],    (1, 2),                'block-list in the last multi-dim dimension';
is-deeply @m[*; {0,1}],    (1, 2, 4, 5, 7, 8),    'block-list flattens under a * dimension';
is-deeply @m[*; 0,1],      (1, 2, 4, 5, 7, 8),    'plain list in last dimension unchanged';

# --- Range receiver ---
is-deeply (10..50)[{0,1}], (10, 11),   'block-list subscript on a Range';
is-deeply (10..50)[{ 2 }], 12,         'block-scalar subscript on a Range unchanged';
