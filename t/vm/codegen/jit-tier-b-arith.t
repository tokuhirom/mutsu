use v6;
use Test;

# JIT Tier B inline arithmetic edge cases (ADR-0004 J4, vm_jit_tier_b.rs).
# Each helper sub is called in a hot loop so that a JIT-enabled run
# (MUTSU_JIT=on, any threshold <= 300 — e.g. the jit-stress CI job) compiles
# it and executes the Tier B fast paths; with the JIT off the same
# assertions pin the interpreter fast paths, so the file passes either way.
# The interesting inputs are exactly the fast-path boundaries: the 48-bit
# small-Int range, Int overflow into big integers, NaN/-0e0 float semantics,
# mixed Int/Num operands, and user-declared infix overrides.

plan 21;

sub add2($a, $b) { $a + $b }
sub sub2($a, $b) { $a - $b }
sub mul2($a, $b) { $a * $b }
sub less2($a, $b) { $a < $b }
sub le2($a, $b) { $a <= $b }
sub eq2($a, $b) { $a == $b }
sub ne2($a, $b) { $a != $b }
sub sel2($a, $b) { if $a && $b { "both" } elsif $a || $b { "one" } else { "none" } }

my $sink;
for ^300 {
    $sink = add2(1, 2);
    $sink = sub2(5, 3);
    $sink = mul2(3, 4);
    $sink = less2(1, 2);
    $sink = le2(1, 1);
    $sink = eq2(1, 1);
    $sink = ne2(1, 2);
    $sink = sel2(True, False);
}

# -- small-Int boundary: results leaving the 48-bit inline range must box --
is add2(2**47 - 1, 1), 2**47, 'Int add crossing the small-int boundary';
is add2(-(2**47), -1), -(2**47) - 1, 'Int add crossing the negative boundary';
is sub2(-(2**47), 1), -(2**47) - 1, 'Int sub crossing the negative boundary';
is add2(2**47 - 1, -(2**47)), -1, 'boundary operands with in-range result';
is mul2(2**40, 2**40), 2**80, 'Int mul overflowing to big integers';
is mul2(2**24, 2**24), 2**48, 'Int mul just past the small-int range';
is mul2(-(2**23), 2**24), -(2**47), 'Int mul landing exactly on the boundary';

# -- Num semantics: NaN, -0e0, infinities --
is add2(1.5e0, 2.5e0), 4e0, 'Num + Num';
ok add2(Inf, -Inf).isNaN, 'Inf + -Inf is NaN';
ok eq2(-0e0, 0e0), '-0e0 == 0e0';
nok eq2(NaN, NaN), 'NaN == NaN is False';
ok ne2(NaN, NaN), 'NaN != NaN is True';
nok less2(NaN, 1e0), 'NaN < Num is False';
nok less2(1e0, NaN), 'Num < NaN is False';

# -- mixed Int/Num falls through to the exact slow path --
is add2(1, 2.5e0), 3.5, 'Int + Num';
ok less2(1, 1.5e0), 'Int < Num';

# -- Bool-driven branches (JumpIfFalse / JumpIfTrue fast paths) --
is sel2(True, True), 'both', '&& chain with both true';
is sel2(True, False), 'one', '|| chain after failed &&';
is sel2(False, False), 'none', 'both branches false';

# -- comparisons across the full small-int range --
ok less2(-(2**47), 2**47 - 1), 'ordered compare across the whole range';
ok le2(2**47 - 1, 2**47 - 1), 'boundary value <= itself';
