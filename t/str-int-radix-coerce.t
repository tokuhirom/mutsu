use Test;

# `Str.Int` / `Str.UInt` parse the string through the full numeric grammar and
# then truncate toward zero, so radix (`:16<ff>`) and rational (`3/4`) string
# forms coerce like their numeric value — not just plain decimal integers.

plan 20;

# --- radix string forms ---
is ":16<ff>".Int, 255,   'hex radix string .Int';
is ":2<101>".Int, 5,     'binary radix string .Int';
is ":8<17>".Int, 15,     'octal radix string .Int';
is ":36<z>".Int, 35,     'base-36 radix string .Int';
is ":16<DEAD_BEEF>".Int, 3735928559, 'radix string with underscores .Int';
is ":16<ff.8>".Int, 255, 'radix string with fractional part truncates .Int';
is "  :16<ff>  ".Int, 255, 'radix string with surrounding whitespace .Int';

# --- rational / decimal string forms truncate toward zero ---
is "3/4".Int, 0,   'rational string .Int truncates';
is "7/2".Int, 3,   'rational string .Int truncates (7/2 -> 3)';
is "3.7".Int, 3,   'decimal string .Int truncates';
is "-3.7".Int, -3, 'negative decimal string .Int truncates toward zero';

# --- plain integer / prefixed forms still work ---
is "10".Int, 10,     'plain decimal .Int';
is "0xff".Int, 255,  '0x prefix .Int';
is "1_000".Int, 1000, 'underscore-separated .Int';

# --- UInt gets the same numeric-string handling ---
is ":16<ff>".UInt, 255, 'hex radix string .UInt';
is "3/4".UInt, 0,       'rational string .UInt';
is "3.7".UInt, 3,       'decimal string .UInt';

# --- invalid / non-finite still fail (as Failures) ---
ok "foo".Int ~~ Failure,  'non-numeric string .Int is a Failure';
ok "Inf".Int ~~ Failure,  'Inf string .Int is a Failure';
is "Inf".Int.exception.^name, 'X::Numeric::CannotConvert',
    'Inf string .Int fails with X::Numeric::CannotConvert';
