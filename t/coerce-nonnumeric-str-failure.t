use v6;
use Test;

# Coercing a non-numeric string to Complex or FatRat must produce the same lazy
# X::Str::Numeric Failure that `.Int`/`.Num` already do — not a bogus 0 value.
# (raku-doc/doc/Type/Cool.rakudoc examples for `.Complex` and `.FatRat`.)

plan 14;

# The soft Failure surfaces as `Failure` from `.^name` (not thrown until used).
is "foo".Complex.^name, 'Failure', 'non-numeric Str.Complex is a Failure';
is "foo".FatRat.^name,  'Failure', 'non-numeric Str.FatRat is a Failure';
is "12abc".Complex.^name, 'Failure', 'trailing-garbage Str.Complex is a Failure';
is "12abc".FatRat.^name, 'Failure', 'trailing-garbage Str.FatRat is a Failure';
# An empty string coerces to 0 (raku parity), not a Failure.
is "".Complex, <0+0i>, 'empty Str.Complex is 0+0i';
is "".FatRat, 0,       'empty Str.FatRat is 0';

# Sinking / using the Failure throws X::Str::Numeric.
throws-like { "foo".Complex.Str }, X::Str::Numeric, 'using the Complex Failure throws';
throws-like { "foo".FatRat.Str }, X::Str::Numeric, 'using the FatRat Failure throws';

# Genuine numeric strings still coerce correctly.
is "1+2i".Complex, <1+2i>,   'numeric string still coerces to Complex';
is <1.3>.Complex, 1.3+0i,    'RatStr coerces to Complex';
is "3/4".FatRat, 0.75,       'fraction string coerces to FatRat';
is "1.3".FatRat, 1.3,        'decimal string coerces to FatRat';
is "1e2".FatRat, 100,        'exponent string coerces to FatRat';
is "1.3".FatRat.^name, 'FatRat', 'FatRat coercion result type is FatRat';
