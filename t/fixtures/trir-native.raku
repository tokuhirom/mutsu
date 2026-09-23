# The ops ADR-0116's native lowering emits inline rather than stepping
# through the interpreter: native int and num arithmetic, comparisons,
# shifts, sized-int wrapping, the `is rw` references, the short-circuit jumps
# that keep their operand, backward jumps, and slots handed to a callee.
#
# `t/vm/codegen/adr0116-trir-native.t` runs this file with the lowering on at
# a threshold of 1 (every chunk native from its first run), with it off, and
# with TRIR off, and requires all three to agree. Every routine is called
# more than once, so a first-run-only answer cannot pass.
use nqp;

my sub int-ops(int $a, int $b) {
    my int $r = nqp::add_i($a, $b);
    $r = nqp::add_i(nqp::mul_i($r, 31), nqp::sub_i($a, $b));
    $r = nqp::bitxor_i($r, nqp::bitand_i($a, 255));
    $r = nqp::bitor_i($r, nqp::bitshiftl_i($b, 3));
    $r = nqp::add_i($r, nqp::bitshiftr_i(nqp::neg_i($a), 2));
    $r
}

my sub cmp-ops(int $a, int $b) {
    nqp::add_i(nqp::mul_i(nqp::islt_i($a, $b), 1),
      nqp::add_i(nqp::mul_i(nqp::isle_i($a, $b), 2),
        nqp::add_i(nqp::mul_i(nqp::isgt_i($a, $b), 4),
          nqp::add_i(nqp::mul_i(nqp::isge_i($a, $b), 8),
            nqp::add_i(nqp::mul_i(nqp::iseq_i($a, $b), 16),
              nqp::mul_i(nqp::isne_i($a, $b), 32))))))
}

my sub wraps(int $a, int $b) { nqp::mul_i($a, $b) }

my sub num-ops(num $x, num $y) {
    my num $s = nqp::add_n($x, $y);
    $s = nqp::mul_n($s, nqp::sub_n($x, $y));
    $s = nqp::div_n($s, 4e0);
    $s
}

my sub num-cmp(num $x, num $y) {
    nqp::add_i(nqp::islt_n($x, $y),
      nqp::add_i(nqp::mul_i(nqp::isgt_n($x, $y), 2),
        nqp::mul_i(nqp::iseq_n($x, $y), 4)))
}

my sub sized(int $v) {
    my uint8 $u = $v;
    my int16 $s = $v;
    nqp::add_i(nqp::mul_i($u, 100000), $s)
}

# A counted loop: a backward jump, a slot incremented each iteration.
my sub tri(int $n) {
    my int $i = 0;
    my int $acc = 0;
    nqp::while(nqp::isle_i($i, $n), nqp::stmts(($acc = nqp::add_i($acc, $i)), ++$i));
    $acc
}

# `&&` / `||` yield an operand: the jump keeps it on the bank.
my sub short(int $a, int $b) { nqp::add_i(nqp::mul_i(($a && $b), 10), ($a || $b)) }

# An `is rw` parameter written through its reference, and handed on.
my sub bump(int $p is rw) { ++$p; $p = nqp::add_i($p, 2); --$p; $p }
my sub bump-twice(int $p is rw) { bump($p); bump($p); $p }

# A slot passed to a callee that writes it, then read after the call.
my sub caller-slot(int $start) {
    my int $x = $start;
    bump($x);
    my int $y = nqp::mul_i($x, 2);
    bump($y);
    nqp::add_i(nqp::mul_i($x, 1000), $y)
}

# The same string scan JSON::Fast's `nom-ws` is.
my $ws := nqp::list_i;
nqp::bindpos_i($ws, 32, 1);
nqp::bindpos_i($ws, 10, 1);
my sub skip-ws(str $text, int $pos is rw) {
    nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
    $pos
}

my sub divide(int $a, int $b) { nqp::div_i($a, $b) }

for ^2 {
    say "int-ops=", int-ops(1234567, -89), " ", int-ops(-5, 7);
    say "cmp-ops=", cmp-ops(1, 2), " ", cmp-ops(2, 2), " ", cmp-ops(3, 2);
    say "wraps=", wraps(9223372036854775807, 2), " ", wraps(-9223372036854775807, 3);
    say "num-ops=", num-ops(3.5e0, 1.25e0), " ", num-ops(-2e0, 0.5e0);
    say "num-cmp=", num-cmp(1e0, 2e0), " ", num-cmp(2e0, 1e0), " ", num-cmp(2e0, 2e0);
    say "sized=", sized(300), " ", sized(-1), " ", sized(40000);
    say "tri=", tri(10), " ", tri(1000);
    say "short=", short(0, 5), " ", short(3, 0), " ", short(3, 4);
    my int $p = 5;
    say "bump=", bump($p), " ", $p;
    say "bump-twice=", bump-twice($p), " ", $p;
    say "caller-slot=", caller-slot(7);
    my int $pos = 0;
    say "skip-ws=", skip-ws("  \n x", $pos), " ", $pos;
    say "divide=", divide(17, 5), " ", divide(-17, 5);
    say "divide-by-zero=", (try divide(1, 0)) // "died";
}

# Raku's `%` takes the divisor's sign, `nqp::mod_i` the dividend's, and
# `nqp::div_i` floors: three answers TRIR once got wrong for negative operands.
my sub raku-mod(int $a, int $b) { $a % $b }
my sub nqp-mod(int $a, int $b) { nqp::mod_i($a, $b) }
for ^2 {
    say "raku-mod=", raku-mod(-17, 5), " ", raku-mod(17, -5), " ", raku-mod(17, 5);
    say "nqp-mod=", nqp-mod(-17, 5), " ", nqp-mod(17, -5), " ", nqp-mod(17, 5);
}
