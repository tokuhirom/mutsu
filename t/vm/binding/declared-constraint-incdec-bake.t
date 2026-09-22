use Test;

# #8877 (second consumer, after the `SetLocal` store path in
# `t/vm/binding/declared-constraint-slot-bake.t`): `++`/`--` on a typed lexical
# also probed `__mutsu_type::<name>` through the env chain on every iteration,
# to answer the same question `wrap_native_int_arithmetic_result_for` always
# asked. `wrap_native_int_arithmetic_result_for_slot` answers it from
# `BindingDesc::declared_constraint` first when the opcode carries a
# compile-time-resolved local slot, falling back to the env probe exactly as
# before for anything the bake cannot describe.
#
# Every case below is a shape where a wrong fast answer would diverge from the
# probe: a narrow width that must still wrap (including through a BigInt
# overflow), a subset that redirects a native name, and a slot the bake never
# recorded a constraint for.

plan 15;

# -- the baked natives: postfix/prefix ++/-- are the identity for the wrap --
my int $i = 5;
$i++;
is $i, 6, 'postfix ++ on an int slot';
--$i;
is $i, 5, 'prefix -- on an int slot';

my int64 $i64 = 1;
++$i64;
is $i64, 2, 'prefix ++ on an int64 slot (baked as the same family as int)';

my str $s = "a";
$s++;
is $s, "b", 'postfix ++ on a str slot (succ, not native-int wrapping)';

my num $n = 1e0;
$n++;
is $n, 2e0, 'postfix ++ on a num slot';

# -- a narrow width is NOT the identity: ++/-- must still wrap --
my int8 $b = 126;
$b++;
$b++;
is $b, -128, 'postfix ++ wraps a narrow int8 slot on overflow';

my int8 $b2 = 126;
++$b2;
++$b2;
is $b2, -128, 'prefix ++ wraps a narrow int8 slot on overflow';

my int8 $b3 = -127;
$b3--;
$b3--;
is $b3, 127, 'postfix -- wraps a narrow int8 slot on underflow';

# -- a full-width int slot still wraps once arithmetic overflows i64, which
# routes through the BigInt arm of the same wrap function --
my int $huge = 9223372036854775807;
$huge++;
is $huge, -9223372036854775808, 'a full-width int slot wraps at the i64 boundary too';

# -- one slot, two constraints: the bake must poison and defer to the env,
# same as the store path (a nested `my` reuses the outer slot by default) --
{ my int $p = 1; $p++; is $p, 2, 'first of two same-named typed decls is int'; }
{ my int8 $p = 126; $p++; $p++; is $p, -128, 'second of two same-named typed decls is int8'; }

# -- an untyped slot never consults the bake or the env probe's typed branch --
my $u = 0;
$u++;
is $u, 1, 'postfix ++ on an untyped slot';

# -- an indexed incdec (no compile-time-resolved local slot) still falls
# through to the ordinary probe and is unaffected by the bake --
my int8 @a = 127, 1;
@a[0]++;
is @a[0], -128, 'indexed postfix ++ on a native int8 array element still wraps';

# -- a loop: the point of the whole exercise is that ++ repeats --
my int $acc = 0;
for 1..100 -> $e { $acc++ }
is $acc, 100, 'a hot typed-native ++ loop still accumulates correctly';

# -- a subset can redirect a native constraint name, which is why the bake is
# read only after the same subset guard the store path uses. `num`, not
# `int`: mutsu's subset scoping is process-wide rather than block-scoped
# (`declared-constraint-slot-bake.t` avoids `int` for the same reason), and
# Test.rakumod's own internal counters are `int`-typed -- redirecting `int`
# for the rest of the process breaks `done-testing` itself.
{
    subset num of Int where * > 100;
    my num $r = 500;
    $r++;
    is $r, 501, 'a subset named `num` redirects the native name, ++ still works';
}
