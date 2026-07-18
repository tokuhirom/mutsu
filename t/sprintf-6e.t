use v6.e.PREVIEW;
use Test;

# Pin the 6.e sprintf flag semantics. Under 6.e:
#  - the sign is emitted before the radix prefix (`-0x100`, not `0x-100`);
#  - `+`/space flags do not apply to radix conversions (%b/%o/%x);
#  - `#` forces a trailing decimal point on %e/%f even at precision 0;
#  - the `0` flag zero-pads strings, and beats `-` on float specs.
# The 6.d behavior is pinned by the whitelisted roast/6.d/S32-str/sprintf-*.t
# suite (the language version is lexical to the whole file, so 6.d cases cannot
# live here).

plan 34;

# sign before radix prefix (# with negatives)
is sprintf("%#x", -256), "-0x100", '%#x negative: sign before 0x';
is sprintf("%#X", -256), "-0X100", '%#X negative: sign before 0X';
is sprintf("%#o", -256), "-0400",  '%#o negative: sign before 0';
is sprintf("%#b", -4),   "-0b100", '%#b negative: sign before 0b';

# zero-pad with negative radix values: sign is threaded out
is sprintf("%08x", -4),   "-0000004", '%08x negative zero-pad';
is sprintf("%08o", -4),   "-0000004", '%08o negative zero-pad';
is sprintf("%#08x", -256), "-0x00100", '%#08x negative zero-pad after prefix';
is sprintf("%08b", -4),   "-0000100", '%08b negative zero-pad';

# precision on radix negatives pads digits only
is sprintf("%.3o", -4), "-004", '%.3o negative precision';
is sprintf("%.3x", -4), "-004", '%.3x negative precision';
is sprintf("%#.3x", -4), "-0x004", '%#.3x negative precision';

# +/space flags do NOT apply to radix conversions
is sprintf("%+b", 1), "1",  '%+b: no plus';
is sprintf("% b", 1), "1",  '% b: no space';
is sprintf("%+o", 1), "1",  '%+o: no plus';
is sprintf("% x", 1), "1",  '% x: no space';
is sprintf("%#+b", 4), "0b100", '%#+b: no plus, keeps prefix';

# +/space still apply to decimal
is sprintf("%+d", 1), "+1", '%+d: plus applies to decimal';
is sprintf("% d", 1), " 1", '% d: space applies to decimal';

# # forces the decimal point on %f / %e at precision 0
is sprintf("%#.0f", 0),   "0.",     '%#.0f zero';
is sprintf("%#.0f", 3.5),  "4.",    '%#.0f rounds then forces point';
is sprintf("%#.0f", -256), "-256.", '%#.0f negative';
is sprintf("%#.0e", 1),    "1.e+00", '%#.0e';
is sprintf("%#.0e", 0),    "0.e+00", '%#.0e zero';
is sprintf("%# .0f", 0),   " 0.",    '%# .0f keeps space then point';

# 0 flag zero-pads strings
is sprintf("%08.2s", "Foo"), "000000Fo", '%08.2s zero-pads string';
is sprintf("%08s", "Foo"),   "00000Foo", '%08s zero-pads string';
is sprintf("%08.2s", -4),    "000000-4", '%08.2s zero-pads number-string, no sign threading';
is sprintf("%-08.2s", "Foo"), "Fo      ", '%-08.2s: - beats 0 for strings';

# 0 beats - for float specs (no left-align)
is sprintf("%-08.2f", 2.71), "00002.71",     '%-08.2f: 0 beats - on float';
is sprintf("%-012.2f", 0),   "000000000.00", '%-012.2f wide: 0 beats -';
is sprintf("%-08.2f", -2.71), "-0002.71",     '%-08.2f negative: 0 beats -';
is sprintf("%-012.2e", 0),   "00000.00e+00", '%-012.2e: 0 beats - on scientific';

# - still beats 0 for integer/string specs
is sprintf("%-08d", 42), "42      ", '%-08d: - beats 0 for integers';
is sprintf("%-08b", 42), "101010  ", '%-08b: - beats 0 for radix';
