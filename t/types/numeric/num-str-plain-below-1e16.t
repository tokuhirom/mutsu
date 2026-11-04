use Test;

plan 14;

# Rakudo prints an integer-valued or fractional Num in plain decimal up to
# (not including) 1e16, and switches to scientific notation from there.
is ~1e15,   '1000000000000000', '1e15';
is ~1.5e15, '1500000000000000', '1.5e15';
is ~(2**53 * 1e0), '9007199254740992', '2**53 as a Num';
is ~1234567890123456e0, '1234567890123456', 'a 16-digit Num';
is ~1000000000000000.5e0, '1000000000000000.5', 'a fractional Num above 1e15';
is ~(1e15 + 1), '1000000000000001', '1e15 + 1';
is ~-1.5e15, '-1500000000000000', 'negative';
is ~1e16, '1e+16', '1e16 is scientific';
is ~1.5e16, '1.5e+16', '1.5e16 is scientific';
is ~9999999999999999e0, '1e+16', 'the Num nearest 1e16 is scientific';
is (1e15).raku, '1000000000000000e0', '.raku adds e0';
is [1e15, 2e15].raku, '[1000000000000000e0, 2000000000000000e0]', '.raku inside a list';
is "{1e15}", '1000000000000000', 'string interpolation';
is (1e15).gist, '1000000000000000', '.gist';
