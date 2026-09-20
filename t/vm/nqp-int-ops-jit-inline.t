use Test;
use nqp;

# The Tier B JIT emits `nqp::*_i` binary ops inline (two NaN-box words, one
# machine instruction) instead of calling `exec_nqp_op`. The inline path only
# fires for two small inline Int operands whose result stays in the small-Int
# range; everything else must fall to the unchanged interpreter arm. Every case
# below is one of those boundaries, driven inside a hot loop so the chunk is
# actually JIT-compiled.

plan 22;

# --- the loop the inline path exists for ---
my int $i = 0;
my $acc = 0;
while nqp::islt_i($i, 2000) {
    $acc = nqp::add_i($acc, nqp::mul_i($i, 3));
    $acc = nqp::sub_i($acc, 1);
    $i = nqp::add_i($i, 1);
}
is $i, 2000, 'nqp::islt_i/add_i drive a loop to its bound';
is $acc, 5995000, 'nqp::add_i/sub_i/mul_i accumulate correctly over 2000 iterations';

# --- the small-Int range boundary ---
# 2**47 - 1 is the largest value a NaN-box small Int can hold, so a result past
# it cannot be packed inline and has to reach the boxing interpreter arm.
my $big = 2 ** 47 - 1;
is nqp::add_i($big, 10), 140737488355337, 'add_i past the small-Int ceiling still boxes';
is nqp::sub_i(-$big, 10), -140737488355337, 'sub_i past the small-Int floor still boxes';
is nqp::mul_i($big, 3), 422212465065981, 'mul_i past the small-Int ceiling still boxes';
is nqp::add_i($big, 0), $big, 'the largest small Int is still exact';

# nqp int ops wrap at 64 bits rather than promoting, which only the interpreter
# arm can produce -- the inline path must decline rather than pack a truncation.
is nqp::add_i(9223372036854775807, 1), -9223372036854775808, 'add_i wraps at i64';

# --- operands that are not small Ints ---
# `iarg` coerces through `to_int`, so the inline path must decline on any word
# that is not an Int page rather than reinterpret its bits.
is nqp::add_i("41", 1), 42, 'a Str operand is coerced, not misread';
is nqp::add_i(41.7, 1), 42, 'a Rat operand is coerced, not misread';
my $c = 7;
my $r := $c;
is nqp::add_i($r, 1), 8, 'a bound container operand is dereferenced';

# --- comparisons yield nqp ints, not Bool ---
is nqp::iseq_i(3, 3), 1, 'iseq_i true is int 1';
is nqp::iseq_i(3, 4), 0, 'iseq_i false is int 0';
is nqp::isne_i(3, 3), 0, 'isne_i';
is nqp::islt_i(-5, -4), 1, 'islt_i is signed';
is nqp::isgt_i(-5, -4), 0, 'isgt_i is signed';
is nqp::isle_i(4, 4), 1, 'isle_i at equality';
is nqp::isge_i(3, 4), 0, 'isge_i';
isa-ok nqp::iseq_i(1, 1), Int, 'a comparison result is an Int, not a Bool';

# --- bit ops, including across the sign boundary ---
is nqp::bitand_i(12, 10), 8, 'bitand_i';
is nqp::bitor_i(12, 10), 14, 'bitor_i';
is nqp::bitxor_i(-1, -1), 0, 'bitxor_i of two negatives';
# -256 and 15 are sign-extended differently; the result must stay negative.
is nqp::bitor_i(-256, 15), -241, 'bitor_i keeps the sign extension';
