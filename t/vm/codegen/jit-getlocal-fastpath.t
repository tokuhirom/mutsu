use v6;
use Test;

# Tier B GetLocal inline fast path (ADR-0004 J4d): hot JIT'd bodies read
# plain scalar locals as raw NaN-box words. These pin the fast set (Int,
# Num, Bool, Package) and every dynamic spoiler that must force the shim:
# ContainerRef cells (`:=` / `is rw`), boxed (non-word) values like Str,
# and Nil-slot declaration checks. Run under the default JIT; CI's
# jit-stress job re-runs with MUTSU_JIT_THRESHOLD=2.
#
# Tests 9-11 pin the two probes the emitted guard was reduced to (#7737):
# the single `LOCAL_READ_SPOILERS` latch, which every spoiler source must
# still bump (atomic variables and sigilless attributes had only a
# per-interpreter flag before), and the ordered page-range test, which must
# keep admitting exactly Int/Num/Bool/Package -- Whatever sits one kind id
# below Bool and must still take the shim.

plan 11;

# 1. Plain Int local, hot recursive body (fast path shape).
sub sum-to($n) {
    if $n < 1 { return 0 }
    return $n + sum-to($n - 1);
}
my $r = 0;
for ^150 { $r = sum-to(60) }
is $r, 1830, 'hot Int local reads stay correct';

# 2. Num local in a hot loop body.
my $x = 0.5e0;
for ^300 { $x = 3.9e0 * $x * (1e0 - $x) }
ok $x > 0e0 && $x < 1e0, 'hot Num local reads stay in range';

# 3. Bool local read in a hot body.
sub flip-count($n) {
    my $b = True;
    my $c = 0;
    for ^$n { $c = $c + ($b ?? 1 !! 0); $b = !$b }
    return $c;
}
is flip-count(200), 100, 'hot Bool local reads stay correct';

# 4. Str local (boxed word — must take the shim, values intact).
sub tag($n) {
    my $prefix = "v";
    return $prefix ~ $n;
}
my $s = "";
for ^150 { $s = tag($_) }
is $s, "v149", 'boxed Str locals still read correctly';

# 5. `is rw` parameter (ContainerRef cell spoiler): the caller's slot and
#    the callee's param share a cell, so post-call reads must see the write.
sub bump($v is rw) { $v = $v + 1 }
my $acc = 0;
for ^200 { bump($acc) }
is $acc, 200, 'is-rw cell writes visible through hot caller reads';

# 6. `:=` bound alias read in a hot loop: both names track one cell.
my $src = 0;
my $alias := $src;
for ^200 { $src = $src + 1; $r = $alias }
is $r, 200, 'bound alias reads track the shared cell';

# 7. Type-object (Package word) local read in a hot body.
sub type-name($n) {
    my $t = Int;
    return $t.^name ~ $n;
}
my $tn = "";
for ^150 { $tn = type-name($_) }
is $tn, "Int149", 'Package-word locals read correctly';

# 8. Nil-slot read (undeclared-check branch must still fire via the shim):
#    a declared-but-Nil local reads as Any, not a crash.
sub niler() {
    my $u;
    return $u.defined;
}
my $d = True;
for ^150 { $d = niler() }
nok $d, 'Nil slots keep the interpreter declaration semantics';

# 9. Atomic-variable spoiler: registering atomic storage must route every
#    later inline read through the shim, whichever interpreter saw it.
my atomicint $counter = 0;
sub read-through($n) {
    my $local = $n;
    return $local + atomic-fetch($counter);
}
my $sum = 0;
for ^150 { atomic-fetch-add($counter, 1); $sum = read-through($_) }
is $sum, 149 + 150, 'atomic storage spoils the inline read without losing values';

# 10. Sigilless attribute alias: the alias table must still be consulted for
#     a bare-name read once any `has $x` attribute has been materialized.
class Holder {
    has $x = 7;
    method bump-sum($n) {
        my $acc = 0;
        for ^$n { $acc = $acc + $x }
        return $acc;
    }
}
is Holder.new.bump-sum(150), 7 * 150, 'sigilless attribute reads stay correct in a hot body';

# 11. Whatever local: one kind id below Bool, so the guard's ordered range
#     must exclude it and let the shim answer.
sub whatever-kind($n) {
    my $w = *;
    return $w.^name ~ $n;
}
my $wn = "";
for ^150 { $wn = whatever-kind($_) }
is $wn, "Whatever149", 'Whatever locals stay off the inline fast path';
