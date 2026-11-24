use Test;

# #18028: `uniname` spells the name of a codepoint without one, and a derived
# ideograph name, with hand-written hex rather than `format!`. A call site
# that remembered its native route answers plain scalar arguments directly;
# these run the same site with every argument kind in turn, so the direct
# answer and the general path must agree.

plan 6;

is-deeply (0x378, 0xE000, 0xFFFF, 0xD800, 0x0, 0x9F, 0x10FFFF, 0x10FFFD, 0xF0000, 0xFDD0).map(&uniname).List,
    <<'<reserved-0378>' '<private-use-E000>' '<noncharacter-FFFF>' '<surrogate-D800>'
      '<control-0000>' '<control-009F>' '<noncharacter-10FFFF>' '<private-use-10FFFD>'
      '<private-use-F0000>' '<noncharacter-FDD0>'>>,
    'the sentinel names';
is-deeply (0x4E00, 0x20000, 0x30000).map(&uniname).List,
    ('CJK UNIFIED IDEOGRAPH-4E00', 'CJK UNIFIED IDEOGRAPH-20000', 'CJK UNIFIED IDEOGRAPH-30000'),
    'derived ideograph names';
is uniname(0x110000), '<unassigned>', 'beyond the last codepoint';
is uniname(-1), '<illegal>', 'a negative codepoint';

my @got;
for 65, "B", 67.0e0, 0x378, "é" -> $arg {
    @got.push: uniname($arg);
}
is-deeply @got, ['LATIN CAPITAL LETTER A', 'LATIN CAPITAL LETTER B',
    'DIGIT SIX', '<reserved-0378>', 'LATIN SMALL LETTER E WITH ACUTE'],
    'one call site, plain scalar arguments of every kind';

my @abs;
for -3, 4.5, -2/3, "-7" -> $arg { @abs.push: abs($arg) }
is-deeply @abs, [3, 4.5, 2/3, 7], 'another builtin site, Int, Num, Rat and Str arguments';
