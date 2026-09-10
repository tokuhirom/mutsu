use Test;

# `uniname` for a codepoint beyond the Unicode maximum (U+10FFFF) has no name;
# Rakudo returns "<unassigned>" rather than throwing. (In-range unassigned
# codepoints return "<reserved-XXXX>" and negatives return "<illegal>".)
# Regression: roast/6.d/S15-unicode-information/uniname.t.

plan 5;

is uniname(0x110000), '<unassigned>', 'just past max returns <unassigned>';
is uniname(0x210000), '<unassigned>', 'far past max returns <unassigned>';
is uniname(0x41), 'LATIN CAPITAL LETTER A', 'assigned codepoint still named';
is uniname(0x378), '<reserved-0378>', 'in-range unassigned still <reserved-XXXX>';
is uniname(-1), '<illegal>', 'negative codepoint still <illegal>';
