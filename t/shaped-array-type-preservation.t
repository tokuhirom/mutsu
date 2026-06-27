use Test;

# Shaped native arrays must keep their element-type (`array[int]`) and shape
# metadata through mutating operations, and render `.raku` in Rakudo's
# `array[T].new(:shape(...), ...)` form. Element-type metadata is embedded in
# ArrayData; the for-loop / map writebacks used to rebuild the array with a
# fresh ArrayData (dropping the metadata), turning `array[int]` into a bare
# `Array` and breaking `.WHAT` / `.raku` / shaped `:delete`-dies behaviour.

plan 9;

# --- .raku format ---------------------------------------------------------
my int @r1[4] = 1, 2, 3, 4;
is @r1.raku, 'array[int].new(:shape(4,), [1, 2, 3, 4])', '1-D int .raku';

my str @rs[2] = "x", "y";
is @rs.raku, 'array[str].new(:shape(2,), ["x", "y"])', '1-D str .raku';

my int @r2[2;2] = (1, 2), (3, 4);
is @r2.raku, 'array[int].new(:shape(2, 2), [1, 2], [3, 4])', '2-D int .raku';

# --- type/shape preserved through a mutating for loop ---------------------
my int @f[4] = 10, 15, 12, 16;
$_++ for @f;
ok @f ~~ Array, 'for-loop: still an array';
is @f.WHAT.^name, 'array[int]', 'for-loop preserves array[int] type';
is @f.raku, 'array[int].new(:shape(4,), [11, 16, 13, 17])', 'for-loop preserves .raku form';
# a shaped array still rejects :delete after a mutating for loop
dies-ok { @f[0]:delete }, 'shaped array still dies on :delete after for-loop mutation';

# --- type preserved through a mutating map --------------------------------
my int @m[3] = 1, 2, 3;
@m.map(* *= 2);
is @m.WHAT.^name, 'array[int]', 'map preserves array[int] type';
is @m.raku, 'array[int].new(:shape(3,), [2, 4, 6])', 'map preserves .raku form';
