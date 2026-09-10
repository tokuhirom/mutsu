use v6.e.PREVIEW;
use Test;

# Pins the multi-dimensional slice lvalue / `||` subscript / `:delete:k` fixes
# exercised by roast/S32-array/multislice-6e.t.

plan 17;

my @array;
sub set-up { @array = [[[42,666,[314]],],]; }

# --- A raw `\target` bound to a multi-dim slice lvalue distributes on assign ---
sub assign-through(\target, \values) { target = values }

# single scalar leaf (existing)
set-up;
is-deeply assign-through(@array[0;0;0], 999), 999, 'scalar leaf assign returns value';
is-deeply @array, [[[999,666,[314]],],], 'scalar leaf mutated in place';

# list slice dimension over existing leaves distributes element-wise
set-up;
is-deeply assign-through(@array[0;0;(0,1,2)], (7,8,9)), (7,8,9), 'slice assign returns list';
is-deeply @array, [[[7,8,9],],], 'slice distributed element-wise';

# whatever slice dimension
set-up;
assign-through(@array[*;0;0], (777,));
is-deeply @array, [[[777,666,[314]],],], 'whatever-dim slice mutated leaf';

# missing scalar leaf autovivifies through the raw bind
set-up;
assign-through(@array[0;0;3], 999);
is-deeply @array, [[[42,666,[314],999],],], 'missing leaf autovivified';
set-up;
assign-through(@array[1;0;0], 999);
is-deeply @array, [[[42,666,[314]],],[[999],]], 'missing top-level autovivified';

# The write must be visible immediately (inside the callee), not deferred.
sub check-inside(\target, \values, @expect) {
    target = values;
    is-deeply @array, @expect, 'write visible immediately inside callee';
}
set-up;
check-inside(@array[0;0;(0,1,2)], (1,2,3), [[[1,2,3],],]);

# ... and through a captured closure (subtest-style capture)
sub via-closure(\target, \values, @expect) {
    my $r;
    { $r = (target = values) }
    is-deeply @array, @expect, 'write visible through captured closure';
}
set-up;
via-closure(@array[0;0;(0,1,2)], (4,5,6), [[[4,5,6],],]);

# --- `||` spreads a list into subscript dimensions ---
{
    my @a;
    my @indices := (0,1), 0;
    is-deeply (@a[|| @indices] = 42, 666), (42,666), '|| assign returns values';
    is-deeply @a, [[42],[666]], '|| initialised nested arrays';
    is-deeply (@a[|| @indices] = 7, 8), (7,8), '|| reassign';
    is-deeply @a, [[7],[8]], '|| reassigned in place';
    is-deeply (@a[|| @indices]:delete), (7,8), '|| :delete returns values';
    is-deeply @a, [[],[]], '|| :delete removed leaves';
    is-deeply @a[|| 1], [], '|| single index (emptied leaf)';
}

# --- `:delete:k` / `:delete:kv` / `:delete:p` (delete BEFORE the value adverb) ---
{
    my @m = [1,2,3], [4,5,6], [7,8,9];
    is-deeply @m[*; {1,2}]:delete:k,
      ((0,1),(0,2),(1,1),(1,2),(2,1),(2,2)),
      ':delete:k yields coordinate keys';
}
