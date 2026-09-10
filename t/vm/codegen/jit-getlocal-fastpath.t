use v6;
use Test;

# Tier B GetLocal inline fast path (ADR-0004 J4d): hot JIT'd bodies read
# plain scalar locals as raw NaN-box words. These pin the fast set (Int,
# Num, Bool, Package) and every dynamic spoiler that must force the shim:
# ContainerRef cells (`:=` / `is rw`), boxed (non-word) values like Str,
# and Nil-slot declaration checks. Run under the default JIT; CI's
# jit-stress job re-runs with MUTSU_JIT_THRESHOLD=2.

plan 8;

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
