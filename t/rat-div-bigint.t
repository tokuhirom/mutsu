use v6;
use Test;

# Dividing a Rat by a BigInt (an integer literal that does not fit in a 64-bit
# int) must use exact big-rational arithmetic, degrading to Num when the
# resulting denominator exceeds uint64 — matching Rakudo. Previously mutsu fell
# through to a `0` (Int) result because neither the i64 Rat path nor the
# BigInt-integer path handled the mixed Rat/BigInt pair.

plan 8;

# Denominator overflows uint64 -> degrade to Num (Rakudo gives 4.25e-18).
is (42.5 / 9999999999999999999).^name, 'Num', 'Rat / BigInt overflows to Num';
is-approx (42.5 / 9999999999999999999), 4.25e-18, 'Rat / BigInt value is correct';

# Written as an explicit ratio, same result.
is (85 / 2 / 9999999999999999999).^name, 'Num', '85/2 / BigInt is Num';

# BigInt / Rat keeps a representable Rat (denominator 85 fits).
is (9999999999999999999 / 42.5).^name, 'Rat', 'BigInt / Rat stays Rat';
is (9999999999999999999 / 42.5), 235294117647058823.505882, 'BigInt / Rat value';

# Denominator still fits uint64 after reduction -> stays an exact Rat.
is (42.5 / 10000000000000000000).^name, 'Rat', 'Rat / BigInt within uint64 stays Rat';

# The result is never a bare Int 0.
isnt (42.5 / 9999999999999999999), 0, 'result is not truncated to 0';

# Multiplication with a BigInt was already exact and stays a Rat.
is (42.5 * 9999999999999999999).^name, 'Rat', 'Rat * BigInt stays Rat';
