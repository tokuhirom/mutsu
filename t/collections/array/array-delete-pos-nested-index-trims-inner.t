use Test;

# #10926: a multi-index `.DELETE-POS` deletes on the innermost array like the
# single-dimension form, so deleting an inner array's last element shrinks it.

plan 6;

my @d = [1, 2], [3, 4];
is @d.DELETE-POS(0, 1), 2, 'returns the deleted element';
is-deeply @d[0].List, (1,), 'the inner array lost its trailing slot';
is @d.gist, '[[1] [3 4]]', 'and the outer array prints without a trailing (Any)';

is @d.DELETE-POS(1, 0), 3, 'a non-trailing inner delete returns its element';
is @d[1].elems, 2, 'and keeps the inner length (a hole stays)';

my @e = [1, 2, 3],;
@e.DELETE-POS(0, 1);
is @e[0].elems, 3, 'a middle delete does not shrink';
