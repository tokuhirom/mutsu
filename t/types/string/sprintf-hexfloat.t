use Test;

# The `%a` / `%A` sprintf directives render a Num as a C99 hexadecimal float:
# a `0x` prefix, one hex digit before the radix point, the mantissa in hex
# after it, then `p` and a *decimal* signed binary exponent. Expected values
# below are glibc's printf output for the same double.

plan 38;

# Exact rendering (no precision): the full 52-bit fraction, trailing zeroes
# stripped.
is sprintf("%a", 0e0), "0x0p+0", "%a of 0";
is sprintf("%a", -0e0), "-0x0p+0", "%a keeps the sign of negative zero";
is sprintf("%a", 1e0), "0x1p+0", "%a of 1";
is sprintf("%a", 2e0), "0x1p+1", "%a of 2";
is sprintf("%a", 0.5e0), "0x1p-1", "%a of 0.5";
is sprintf("%a", 27.1e0), "0x1.b19999999999ap+4", "%a of 27.1e0";
is sprintf("%a", -2.71e0), "-0x1.5ae147ae147aep+1", "%a of -2.71e0";

# A Rat argument renders the double it denotes, like every other float
# directive.
is sprintf("%a", 27.1), "0x1.b19999999999ap+4", "%a of the Rat 27.1";
is sprintf("%a", -2.71), "-0x1.5ae147ae147aep+1", "%a of the Rat -2.71";
is sprintf("%a", 0), "0x0p+0", "%a of the Int 0";

# %A is the same with uppercase hex digits, `0X` and `P`.
is sprintf("%A", 27.1), "0X1.B19999999999AP+4", "%A uppercases digits, 0X and P";
is sprintf("%A", -2.71), "-0X1.5AE147AE147AEP+1", "%A of a negative value";

# With a precision the mantissa is rounded to that many fractional hex digits,
# ties-to-even. A carry out of the leading digit is NOT renormalized: C prints
# `0x2p+4`, not `0x1p+5`.
is sprintf("%.0a", 27.1), "0x2p+4", "%.0a rounds up without renormalizing";
is sprintf("%.0a", -2.71), "-0x1p+1", "%.0a rounds down";
is sprintf("%.3a", 27.1), "0x1.b1ap+4", "%.3a rounds the mantissa up";
is sprintf("%.3a", -2.71), "-0x1.5aep+1", "%.3a rounds the mantissa down";
is sprintf("%.3a", 0), "0x0.000p+0", "%.3a of 0 pads the fraction";
is sprintf("%.1a", 1.9999999e0), "0x2.0p+0", "a carry keeps the requested width";
is sprintf("%.20a", 27.1), "0x1.b19999999999a0000000p+4",
  "a precision past the 13-digit fraction pads with zeroes";
is sprintf("%.*a", 3, 27.1), "0x1.b1ap+4", "a star precision is honoured";

# Subnormals are printed at the fixed minimum exponent with a leading 0,
# rather than being normalized.
is sprintf("%a", 5e-324), "0x0.0000000000001p-1022", "%a of the smallest subnormal";
is sprintf("%a", 2.2250738585072014e-308), "0x1p-1022", "%a of the smallest normal";

# The `#` flag forces the radix point even when the fraction is empty.
is sprintf("%#a", 0e0), "0x0.p+0", "%#a of 0 keeps the radix point";
is sprintf("%#.0a", 1e0), "0x1.p+0", "%#.0a keeps the radix point";
is sprintf("%#a", 27.1), "0x1.b19999999999ap+4", "%#a is a no-op when a fraction is present";

# Sign flags.
is sprintf("%+a", 27.1), "+0x1.b19999999999ap+4", "the + flag signs a positive value";
is sprintf("% a", 27.1), " 0x1.b19999999999ap+4", "the space flag pads a positive value";
is sprintf("%+a", -2.71), "-0x1.5ae147ae147aep+1", "the + flag leaves a negative sign alone";

# Width: the `0` flag pads between the `0x` prefix and the mantissa, and is
# ignored when combined with `-`, exactly as in C.
is sprintf("%24a", 27.1), "    0x1.b19999999999ap+4", "a width right-aligns with spaces";
is sprintf("%024a", 27.1), "0x00001.b19999999999ap+4", "the 0 flag pads after the 0x prefix";
is sprintf("%024a", -2.71), "-0x0001.5ae147ae147aep+1", "zero padding goes after the sign too";
is sprintf("%0 24a", 0e0), " 0x000000000000000000p+0", "the space flag shares the field";
is sprintf("%-24a|", 27.1), "0x1.b19999999999ap+4    |", "the - flag left-aligns";
is sprintf("%-024a|", 27.1), "0x1.b19999999999ap+4    |", "- beats 0";

# Inf/NaN use Raku's spelling for every float directive, `%A` included.
is sprintf("%a", Inf), "Inf", "%a of Inf";
is sprintf("%a", -Inf), "-Inf", "%a of -Inf";
is sprintf("%A", NaN), "NaN", "%A of NaN";

# The directive is recognised, so the unsupported-directive exception no
# longer fires for it.
lives-ok { sprintf("%a", 1e0) }, "%a is a supported directive";

# vim: expandtab shiftwidth=4
