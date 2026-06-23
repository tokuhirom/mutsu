use Test;

plan 10;

# A trailing dimension applied to a flat List indexes each selected scalar
# as a 1-element list: `20[0]` is 20, so `(10,20,30)[1,2;0]` is `(20 30)`.
is-deeply (10,20,30)[1,2;0], (20, 30), 'flat List slice with scalar-leaf 0 dimension';
is (10,20,30)[1;0], 20, 'single index with scalar-leaf 0 dimension';
is (10,20,30)[0;0], 10, 'first element with scalar-leaf 0 dimension';
is-deeply (10,20,30)[*;0], (10, 20, 30), 'Whatever first dim with scalar-leaf 0';
is-deeply (10,20,30)[1;*], (20,), 'Whatever leaf dimension on a scalar';
is-deeply (10,20,30)[1,2;0;0], (20, 30), 'two trailing scalar-leaf dimensions';

# Arrays behave identically.
my @a = 10, 20, 30;
is-deeply @a[1,2;0], (20, 30), 'flat Array slice with scalar-leaf 0 dimension';

# Nested arrays were already correct; keep them passing.
is-deeply ((1,2),(3,4))[0,1;1], (2, 4), 'nested AoA slice still works';

# A bare scalar under a positional subscript is a 1-element list.
is 20[0], 20, 'scalar literal indexed by 0 is itself';
is "hi"[0], "hi", 'string literal indexed by 0 is itself';
