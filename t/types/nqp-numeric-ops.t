use v6;
use Test;
use nqp;

# The numeric, transcendental and random `nqp::` ops (#11490) used to die
# with "Unsupported nqp:: op". Every expected value below is what rakudo
# (MoarVM) answers, including its edge cases: lcm_i's sign follows the signed
# gcd, pow_i wraps and answers 0 for a negative exponent, mod_n by zero is the
# dividend, and div_In is the exact quotient as a Num.

plan 12;

subtest 'native int: gcd_i / lcm_i / pow_i', {
    plan 11;
    is nqp::gcd_i(12, -18), 6, 'gcd_i is non-negative';
    is nqp::gcd_i(-12, -18), 6, 'gcd_i of two negatives';
    is nqp::gcd_i(0, 0), 0, 'gcd_i(0, 0)';
    is nqp::lcm_i(4, 6), 12, 'lcm_i';
    is nqp::lcm_i(-4, 6), -12, 'lcm_i sign follows the signed gcd (-4, 6)';
    is nqp::lcm_i(4, -6), 12, 'lcm_i sign follows the signed gcd (4, -6)';
    is nqp::lcm_i(0, 6), 0, 'lcm_i with a zero operand';
    is nqp::pow_i(2, 10), 1024, 'pow_i';
    is nqp::pow_i(2, 64), 0, 'pow_i wraps';
    is nqp::pow_i(3, 41), -420491770248316829, 'pow_i wraps into the negative range';
    is nqp::pow_i(1, -5), 0, 'pow_i with a negative exponent is 0';
}

subtest 'mod_n is floored', {
    plan 6;
    is nqp::mod_n(7e0, 3e0), 1e0, 'positive operands';
    is nqp::mod_n(-7e0, 3e0), 2e0, 'takes the divisor sign (negative dividend)';
    is nqp::mod_n(7e0, -3e0), -2e0, 'takes the divisor sign (negative divisor)';
    is nqp::mod_n(7.5e0, 2e0), 1.5e0, 'fractional';
    is nqp::mod_n(-7.5e0, 0e0), -7.5e0, 'a zero divisor answers the dividend';
    ok nqp::isnanorinf(nqp::mod_n(7e0, Inf)), 'an infinite divisor is NaN';
}

subtest 'pow_n / sqrt_n / exp_n / log_n', {
    plan 8;
    is-approx nqp::pow_n(2e0, 0.5e0), 1.4142135623730951e0, 'pow_n';
    is nqp::pow_n(0e0, -1e0), Inf, 'pow_n(0, -1) is Inf';
    is nqp::sqrt_n(16e0), 4e0, 'sqrt_n';
    is nqp::sqrt_n(-1e0).Str, 'NaN', 'sqrt_n of a negative is NaN';
    isa-ok nqp::sqrt_n(2), Num, 'sqrt_n of an Int answers a Num';
    is-approx nqp::exp_n(1e0), 2.718281828459045e0, 'exp_n';
    is-approx nqp::log_n(10e0), 2.302585092994046e0, 'log_n';
    is nqp::log_n(0e0), -Inf, 'log_n(0) is -Inf';
}

subtest 'ceil_n / floor_n', {
    plan 4;
    is nqp::ceil_n(1.2e0), 2e0, 'ceil_n';
    is nqp::ceil_n(-1.2e0), -1e0, 'ceil_n of a negative';
    is nqp::floor_n(-1.2e0), -2e0, 'floor_n of a negative';
    is nqp::floor_n(Inf), Inf, 'floor_n(Inf)';
}

subtest 'inf / neginf / nan', {
    plan 3;
    is nqp::inf(), Inf, 'inf';
    is nqp::neginf(), -Inf, 'neginf';
    is nqp::nan().Str, 'NaN', 'nan';
}

subtest 'trigonometric', {
    plan 10;
    is nqp::sin_n(0e0), 0e0, 'sin_n';
    is nqp::cos_n(0e0), 1e0, 'cos_n';
    is-approx nqp::tan_n(1e0), 1.5574077246549023e0, 'tan_n';
    is-approx nqp::asin_n(1e0), pi / 2, 'asin_n';
    is nqp::acos_n(1e0), 0e0, 'acos_n';
    is-approx nqp::atan_n(1e0), pi / 4, 'atan_n';
    is-approx nqp::atan2_n(1e0, -1e0), 3 * pi / 4, 'atan2_n takes (y, x)';
    is-approx nqp::sinh_n(1e0), 1.1752011936438014e0, 'sinh_n';
    is nqp::cosh_n(0e0), 1e0, 'cosh_n';
    is-approx nqp::tanh_n(1e0), 0.7615941559557649e0, 'tanh_n';
}

subtest 'div_In: exact quotient of two Ints as a Num', {
    plan 6;
    is nqp::div_In(7, 2), 3.5e0, 'div_In';
    is nqp::div_In(-7, 2), -3.5e0, 'div_In negative';
    is nqp::div_In(10**400, 10**399), 10e0, 'operands beyond the Num range';
    is nqp::div_In(1, 0), Inf, 'positive / 0 is Inf';
    is nqp::div_In(-1, 0), -Inf, 'negative / 0 is -Inf';
    is nqp::div_In(0, 0).Str, 'NaN', '0 / 0 is NaN';
}

subtest 'base_I renders like Int.base', {
    plan 5;
    is nqp::base_I(255, 16), 'FF', 'base_I';
    is nqp::base_I(-255, 2), '-11111111', 'base_I negative';
    is nqp::base_I(10**20, 36), 'L3R41IFS0Q5TS', 'base_I big';
    is nqp::base_I(0, 10), '0', 'base_I zero';
    is nqp::base_I(10**20, 36), (10**20).base(36), 'the same digits as Int.base';
}

subtest 'expmod_I', {
    plan 2;
    is nqp::expmod_I(4, 13, 497, Int), 445, 'expmod_I';
    is nqp::expmod_I(4, 13, 497, Int), expmod(4, 13, 497), 'the same answer as expmod';
}

subtest 'srand / rand_n reproduce a sequence', {
    plan 3;
    is nqp::srand(42), 42, 'srand answers the seed';
    my $first = nqp::rand_n(10e0);
    nqp::srand(42);
    is nqp::rand_n(10e0), $first, 'the same seed gives the same draw';
    ok 0e0 <= $first < 10e0, 'rand_n is in [0, max)';
}

subtest 'rand_I / rand_i', {
    plan 3;
    my @draws = (^50).map({ nqp::rand_I(10, Int) });
    ok @draws.all ~~ 0..9, 'rand_I is in [0, max)';
    ok nqp::rand_I(10**30, Int) < 10**30, 'rand_I with a big bound';
    isa-ok nqp::rand_i(), Int, 'rand_i answers an int';
}

subtest 'operands coerce like the other _n ops', {
    plan 2;
    is nqp::sqrt_n('4'), 2e0, 'a Str operand numifies';
    is nqp::pow_i(3, 3), 27, 'an Int operand';
}
