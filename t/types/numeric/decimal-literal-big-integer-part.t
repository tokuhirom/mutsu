use v6;
use Test;

# A decimal literal whose integer part exceeds i64 must keep it. mutsu's
# `unwrap_or(0)` turned the failed `parse::<i64>()` into a 0 whose
# `0 * 10^frac_digits + frac` did not overflow, so the exact-BigInt fallback --
# selected by arithmetic overflow rather than by the parse -- never ran and
# `1000000000000000000000000000000.5` evaluated to `0.5`.

plan 14;

is 1000000000000000000000000000000.5.Str, '1000000000000000000000000000000.5',
    'an integer part far past i64::MAX survives';
is 12345678901234567890.5.Str, '12345678901234567890.5',
    'and one just past it';
is (-12345678901234567890.5).Str, '-12345678901234567890.5',
    'the sign is kept';
is 12345678901234567890.25.Str, '12345678901234567890.25',
    'two fractional digits are kept';

# The threshold is exactly i64::MAX; the side that already worked must not move.
is 9223372036854775807.5.Str, '9223372036854775807.5', 'i64::MAX is unchanged';
is 9223372036854775808.5.Str, '9223372036854775808.5', 'i64::MAX + 1 now works';

# The literal is a Rat in raku, and an exact one.
isa-ok 1000000000000000000000000000000.5, Rat, 'the literal is still a Rat';
is 1000000000000000000000000000000.5.numerator, 2000000000000000000000000000001,
    'the numerator is exact';
is 1000000000000000000000000000000.5.denominator, 2, 'and so is the denominator';

# Ordinary literals must be untouched.
is 1.5.Str, '1.5', 'a small literal is unchanged';
is 0.5.Str, '0.5', 'a zero integer part is unchanged';
is (.5).Str, '0.5', 'a leading-dot literal is unchanged';

# A fraction too long for an i64 denominator: `10i64.pow(frac_digits)` PANICS
# past 18 digits, so the leading-dot branch has to test before it multiplies.
# Only the exact value is asserted -- `Rat.Str` for an over-64-bit denominator
# is `todo/tickets/big-denominator-rat-str-truncates-to-f64.md`.
is (.1234567890123456789012345).numerator, 246913578024691357802469,
    'a 25-digit fraction parses exactly rather than aborting';
is (.1234567890123456789012345).denominator, 2000000000000000000000000,
    '... with the exact denominator';
