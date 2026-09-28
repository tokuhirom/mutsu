use Test;

# `.Int`/`.Rat`/`.FatRat` on a Complex coerce via the real part for a purely
# real Complex, and throw X::Numeric::Real for a non-zero imaginary part —
# matching `.Num`/`.Real` (see t/types/numeric/numeric-real-target.t) and
# raku's own message:
#   say (1+2i).Int; CATCH { default { put .^name, ": ", .Str } }
#   # OUTPUT: «X::Numeric::Real: Cannot convert 1+2i to Int: imaginary part not zero␤»
# Regression: `.Int` silently truncated (dropped the imaginary part) instead
# of throwing; `.Rat`/`.FatRat` threw an untyped X::AdHoc instead.

plan 10;

throws-like { (1+2i).Int }, X::Numeric::Real, target => Int,
    'Complex.Int with imaginary part throws X::Numeric::Real';
is (5+0i).Int, 5, '(5+0i).Int -> 5';
is (5.7+0i).Int, 5, '(5.7+0i).Int truncates -> 5';

throws-like { (1-2i).Int }, X::Numeric::Real,
    'Complex.Int with a negative imaginary part throws too';

throws-like { (1+2i).UInt }, X::Numeric::Real, target => UInt,
    'Complex.UInt reports its own target type (UInt, not Real)';

throws-like { (1+2i).Rat }, X::Numeric::Real, target => Rat,
    'Complex.Rat with imaginary part throws X::Numeric::Real';
is (5+0i).Rat, 5, '(5+0i).Rat -> 5';

throws-like { (1+2i).FatRat }, X::Numeric::Real, target => FatRat,
    'Complex.FatRat with imaginary part throws X::Numeric::Real';
is (5+0i).FatRat, 5, '(5+0i).FatRat -> 5';

# The exception message matches raku's exact wording.
throws-like { (1+2i).Int }, X::Numeric::Real,
    message => 'Cannot convert 1+2i to Int: imaginary part not zero';
