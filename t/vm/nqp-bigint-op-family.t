use Test;
use nqp;

# #9344: the big-integer `nqp::*_I` ops. Each one is a call into the shared Int
# home (ADR-0118, src/builtins/arith/), so it must agree with the operator it
# implements in Rakudo. Expected values were measured with rakudo 2026.07.

plan 32;

my Int $big := nqp::decont(2 ** 70);          # 1180591620717411303424
my Int $nbig := nqp::decont(-(2 ** 70));

# -- comparisons answer a native 0/1 --
is nqp::islt_I(1, 2), 1, 'islt_I';
is nqp::isne_I(1, 2), 1, 'isne_I';
is nqp::iseq_I($big, $big), 1, 'iseq_I past 64 bits';
is nqp::isle_I($nbig, 5), 1, 'isle_I with a negative BigInt';
is nqp::isgt_I($big, 3), 1, 'isgt_I';
is nqp::isge_I(-7, 2), 0, 'isge_I';
is nqp::cmp_I(-7, 2), -1, 'cmp_I less';
is nqp::cmp_I($big, 3), 1, 'cmp_I more';
is nqp::cmp_I(3, 3), 0, 'cmp_I same';

# -- arithmetic --
is nqp::add_I($big, 3, Int), 1180591620717411303427, 'add_I';
is nqp::sub_I(3, $big, Int), -1180591620717411303421, 'sub_I';
is nqp::mul_I($big, 3, Int), 3541774862152233910272, 'mul_I';
is nqp::div_I(7, -2, Int), -4, 'div_I floors';
is nqp::div_I(7, -2, Int), 7 div -2, 'div_I agrees with infix:<div>';
is nqp::div_I($nbig, 5, Int), -236118324143482260685, 'div_I floors a negative BigInt';
is nqp::mod_I(-7, 2, Int), 1, 'mod_I takes the divisor sign';
is nqp::mod_I(7, -2, Int), -1, 'mod_I with a negative divisor';
is nqp::mod_I(-7, 2, Int), -7 % 2, 'mod_I agrees with infix:<%>';
is nqp::pow_I(2, 100, Num, Int), 1267650600228229401496703205376, 'pow_I';
is nqp::pow_I(-3, 3, Num, Int), -27, 'pow_I with a negative base';
is nqp::pow_I(2, -1, Num, Int), 0.5, 'pow_I with a negative exponent is a Num';
is nqp::neg_I($nbig, Int), $big, 'neg_I';
is nqp::abs_I($nbig, Int), $big, 'abs_I';
is nqp::gcd_I(7, -2, Int), 1, 'gcd_I';
is nqp::lcm_I(-7, 2, Int), 14, 'lcm_I is non-negative';
is nqp::lcm_I(0, 5, Int), 0, 'lcm_I with a zero operand';

# -- bit ops --
is nqp::bitand_I(-7, 2, Int), 0, 'bitand_I';
is nqp::bitor_I($big, 3, Int), 1180591620717411303427, 'bitor_I';
is nqp::bitxor_I(-7, 2, Int), -5, 'bitxor_I';
is nqp::bitneg_I($big, Int), -1180591620717411303425, 'bitneg_I';
is nqp::bitshiftl_I(1, 100, Int), 1267650600228229401496703205376, 'bitshiftl_I';
is nqp::bitshiftr_I(-9, 1, Int), -5, 'bitshiftr_I is arithmetic';
