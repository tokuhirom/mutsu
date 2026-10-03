use v6;
use Test;
use nqp;

# The ordered-string, unsigned and ignore-mark comparison `nqp::` ops, and the
# string bit ops (#11491), used to die with "Unsupported nqp:: op". Expected
# values are rakudo's (MoarVM's).

plan 5;

subtest 'ordered string comparisons', {
    plan 9;
    is nqp::isgt_s('b', 'a'), 1, 'isgt_s';
    is nqp::isgt_s('a', 'a'), 0, 'isgt_s equal';
    is nqp::islt_s('a', 'b'), 1, 'islt_s';
    is nqp::isle_s('a', 'a'), 1, 'isle_s equal';
    is nqp::isge_s('a', 'b'), 0, 'isge_s';
    is nqp::islt_s('Z', 'a'), 1, 'by codepoint: upper case sorts first';
    is nqp::islt_s("e\x[301]", 'f'), 0, 'compares the NFC form (é is after f)';
    is nqp::islt_s('abc', 'abd'), ('abc' lt 'abd') ?? 1 !! 0, 'agrees with lt';
    is nqp::isge_s('b', 'abc'), ('b' ge 'abc') ?? 1 !! 0, 'agrees with ge';
}

subtest 'unsigned comparisons', {
    plan 9;
    is nqp::islt_u(-1, 1), 0, '-1 is the largest uint';
    is nqp::isgt_u(-1, 1), 1, 'isgt_u';
    is nqp::cmp_u(-1, 1), 1, 'cmp_u';
    is nqp::cmp_u(1, -1), -1, 'cmp_u reversed';
    is nqp::cmp_u(5, 5), 0, 'cmp_u equal';
    is nqp::iseq_u(-1, 18446744073709551615), 1, 'iseq_u: -1 and 2**64-1 are the same uint';
    is nqp::isne_u(1, 2), 1, 'isne_u';
    is nqp::isle_u(5, 5), 1, 'isle_u';
    is nqp::isge_u(5, 5), 1, 'isge_u';
}

subtest 'eqatim / eqaticim', {
    plan 5;
    is nqp::eqatim('café', 'cafe', 0), 1, 'eqatim ignores marks';
    is nqp::eqatim('café', 'e', 3), 1, 'eqatim at a position';
    is nqp::eqatim('abc', 'z', 9), 0, 'eqatim past the end';
    is nqp::eqaticim('CAFÉ', 'cafe', 0), 1, 'eqaticim ignores case and marks';
    is nqp::eqatim('CAFÉ', 'cafe', 0), 0, 'eqatim does not ignore case';
}

subtest 'string bit ops', {
    plan 5;
    is nqp::bitand_s('abc', '  '), '  ', 'bitand_s takes the shorter length';
    is nqp::bitor_s('a', '  '), 'a ', 'bitor_s pads to the longer length';
    is nqp::bitxor_s('ab', 'A'), ' b', 'bitxor_s';
    is nqp::bitand_s('é', "\xFF"), 'é' ~& "\xFF", 'bitand_s agrees with ~&';
    is nqp::bitor_s('ab', '  '), 'ab' ~| '  ', 'bitor_s agrees with ~|';
}

subtest 'answers are native ints', {
    plan 2;
    isa-ok nqp::isgt_s('b', 'a'), Int, 'isgt_s';
    isa-ok nqp::cmp_u(1, 2), Int, 'cmp_u';
}
