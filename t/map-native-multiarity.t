use Test;

# Exercises the VM-native `.map` fast path extended to multi-arity blocks and
# explicit (pointy) signatures (src/vm/vm_native_map.rs). Both
# `-> $a, $b { ... }` and the placeholder `{ $^a + $^b }` form consume the
# source in arity-sized chunks; single-arity pointy blocks (`-> $a { ... }`),
# which the scanner previously rejected because their bodies carry a `SetLine`
# marker, now also run natively. Results must match the interpreter and raku.

plan 13;

my @a = 1, 2, 3, 4;

# --- placeholder multi-arity ---
is-deeply @a.map({ $^a + $^b }).List, (3, 7), "placeholder 2-arity sum";
is-deeply @a.map({ $^a * $^b }).List, (2, 12), "placeholder 2-arity product";

# --- pointy multi-arity ---
is-deeply @a.map(-> $a, $b { $a + $b }).List, (3, 7), "pointy 2-arity sum";
is-deeply @a.map(-> $a, $b { "$a-$b" }).List, ("1-2", "3-4"), "pointy 2-arity interpolation";

my @six = 1, 2, 3, 4, 5, 6;
is-deeply @six.map(-> $a, $b, $c { $a + $b + $c }).List, (6, 15), "pointy 3-arity";

# --- single-arity pointy (SetLine in body) ---
is-deeply @a.map(-> $x { $x * 10 }).List, (10, 20, 30, 40), "pointy 1-arity";
is-deeply @a.map(-> $x { $x }).List, (1, 2, 3, 4), "pointy 1-arity identity";

# --- single-arity placeholder still native ---
is-deeply @a.map({ $_ * 2 }).List, (2, 4, 6, 8), "implicit topic 1-arity";
is-deeply @a.map({ $^x + 1 }).List, (2, 3, 4, 5), "named placeholder 1-arity";

# --- Slip flattening from a multi-arity block ---
is-deeply @a.map(-> $a, $b { ($b, $a).Slip }).List, (2, 1, 4, 3), "multi-arity Slip swap";

# --- multi-arity over a longer even list ---
my @n = (1 .. 8).Array;
is-deeply @n.map(-> $a, $b { $a * $b }).List, (2, 12, 30, 56), "2-arity over 8 elements";

# --- combine with a downstream operation ---
is-deeply @a.map(-> $a, $b { $a + $b }).map(* + 100).List, (103, 107), "chained map after multi-arity";

# --- result is a fresh List, source unchanged ---
{
    @a.map(-> $a, $b { $a + $b });
    is-deeply @a.List, (1, 2, 3, 4), "multi-arity map leaves source unchanged";
}
