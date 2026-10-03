use v6;
use Test;
use nqp;

# The native coercion, big-integer conversion and value-test `nqp::` ops
# (#11492) used to die with "Unsupported nqp:: op". Expected values are
# rakudo's (MoarVM's), including its edge cases: coerce_ni gives i64::MIN for
# anything that does not fit, coerce_us renders the 64 bits as signed, and the
# decont_* ops only unbox their own boxed type.

plan 7;

subtest 'native coercions', {
    plan 12;
    is nqp::coerce_in(3), 3e0, 'coerce_in';
    isa-ok nqp::coerce_in(3), Num, 'coerce_in answers a num';
    is nqp::coerce_ni(3.7e0), 3, 'coerce_ni truncates';
    is nqp::coerce_ni(-3.7e0), -3, 'coerce_ni truncates toward zero';
    is nqp::coerce_ni(1e30), -9223372036854775808, 'coerce_ni out of range';
    is nqp::coerce_ni(NaN), -9223372036854775808, 'coerce_ni of NaN';
    is nqp::coerce_ns(1.5e0), '1.5', 'coerce_ns';
    is nqp::coerce_ns(1e16), '1e+16', 'coerce_ns uses the Num.Str form';
    is nqp::coerce_ns(-0e0), '-0', 'coerce_ns of -0';
    is nqp::coerce_iu(-1), 18446744073709551615, 'coerce_iu reads the bits as unsigned';
    is nqp::coerce_ui(18446744073709551615), -1, 'coerce_ui reads the bits as signed';
    is nqp::coerce_us(18446744073709551615), '-1', 'coerce_us renders the bits signed, as MoarVM';
}

subtest 'intify / numify', {
    plan 9;
    is nqp::intify('42'), 42, 'intify a Str';
    is nqp::intify(' 7 '), 7, 'intify skips whitespace';
    is nqp::intify('3.9'), 3, 'intify takes the leading integer';
    is nqp::intify('abc'), 0, 'intify of a non-number is 0';
    is nqp::intify(42), 42, 'intify an Int';
    is nqp::numify('3.5'), 3.5e0, 'numify a Str';
    is nqp::numify(3), 3e0, 'numify an Int';
    is nqp::numify(''), 0e0, 'numify of the empty string is 0';
    dies-ok { nqp::numify('abc') }, 'numify of a non-number dies';
}

subtest 'big-integer conversions', {
    plan 10;
    is nqp::tostr_I(2**70), '1180591620717411303424', 'tostr_I';
    is-approx nqp::tonum_I(2**70), 1.1805916207174113e21, 'tonum_I';
    is nqp::tonum_I(10**400), Inf, 'tonum_I past the Num range';
    is nqp::fromstr_I('123456789012345678901234567890', Int),
        123456789012345678901234567890, 'fromstr_I';
    is nqp::fromstr_I('-12', Int), -12, 'fromstr_I negative';
    is nqp::fromstr_I('', Int), 0, 'fromstr_I of the empty string';
    dies-ok { nqp::fromstr_I('12abc', Int) }, 'fromstr_I rejects trailing garbage';
    is nqp::fromnum_I(1.5e20, Int), 150000000000000000000, 'fromnum_I';
    is nqp::fromnum_I(-1.9e0, Int), -1, 'fromnum_I truncates';
    dies-ok { nqp::fromnum_I(NaN, Int) }, 'fromnum_I of NaN dies';
}

subtest 'big-integer tests', {
    plan 9;
    is nqp::bool_I(0), 0, 'bool_I(0)';
    is nqp::bool_I(-1), 1, 'bool_I(-1)';
    is nqp::isbig_I(2**31 - 1), 0, 'isbig_I inside 32 bits';
    is nqp::isbig_I(2**31), 1, 'isbig_I past 32 bits';
    is nqp::isbig_I(-2**31), 1, 'isbig_I at -2**31 (MoarVM stores it big)';
    is nqp::isbig_I(2**70), 1, 'isbig_I of a BigInt';
    is nqp::isprime_I(7), 1, 'isprime_I';
    is nqp::isprime_I(8), 0, 'isprime_I composite';
    is nqp::fromI_I(42, Int), 42, 'fromI_I';
}

subtest 'boxing', {
    plan 3;
    isa-ok nqp::box_n(1.5e0, Num), Num, 'box_n';
    is nqp::box_n(1.5e0, Num), 1.5e0, 'box_n value';
    is nqp::box_u(-1, Int), 18446744073709551615, 'box_u reads the bits as unsigned';
}

subtest 'decont_*', {
    plan 6;
    my int $i = 5; my str $s = 'x'; my num $n = 1.5e0;
    is nqp::decont_i($i), 5, 'decont_i';
    is nqp::decont_s($s), 'x', 'decont_s';
    is nqp::decont_n($n), 1.5e0, 'decont_n';
    dies-ok { nqp::decont_i('42') }, 'decont_i of a Str dies';
    dies-ok { nqp::decont_s(42) }, 'decont_s of an Int dies';
    dies-ok { nqp::decont_n(3) }, 'decont_n of an Int dies';
}

subtest 'isinvokable / isttyfh', {
    plan 6;
    class WithCallMe { method CALL-ME { } }
    is nqp::isinvokable(sub { }), 1, 'a Sub';
    is nqp::isinvokable(-> { }), 1, 'a Block';
    is nqp::isinvokable(&say), 1, 'a core routine';
    is nqp::isinvokable(1), 0, 'an Int';
    is nqp::isinvokable(WithCallMe.new), 0, 'CALL-ME does not make an object invokable';
    ok nqp::isttyfh(nqp::getstdout()) == 0|1, 'isttyfh answers 0 or 1';
}
