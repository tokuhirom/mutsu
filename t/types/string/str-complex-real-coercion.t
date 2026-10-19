use Test;

# A string that spells a `Complex` converts to a real type as that `Complex`:
# the real part when the imaginary part is negligible under `$*TOLERANCE`, an
# `X::Numeric::Real` naming the number otherwise (ADR-11276 §9.190; every
# answer checked against Rakudo).

plan 15;

is "3.7+0i".Int, 3, '.Int';
is "3.7+0i".UInt, 3, '.UInt';
is "3.7+0i".Num, 3.7, '.Num';
is "3.5+0i".Rat, 3.5, '.Rat';
is "1+0i".Rat.^name, 'Rat', '.Rat is a Rat';
is "3.5+0i".FatRat.raku, 'FatRat.new(7, 2)', '.FatRat';
is "3.5+0i".Real, 3.5, '.Real';
is "1.5e0+1e-20i".Int, 1, 'a negligible imaginary part';

throws-like { "1+2i".Int }, X::Numeric::Real,
    message => 'Cannot convert 1+2i to Int: imaginary part not zero', '.Int of a non-real';
throws-like { "1+2i".Num }, X::Numeric::Real,
    message => 'Cannot convert 1+2i to Num: imaginary part not zero', '.Num of a non-real';
throws-like { "1+2i".UInt }, X::Numeric::Real,
    message => 'Cannot convert 1+2i to Int: imaginary part not zero', '.UInt names Int';
my $real = "1+2i".Real;
isa-ok $real.exception, X::Numeric::Real, '.Real of a non-real is a Failure';
throws-like { "-3.7+0i".UInt }, X::OutOfRange, '.UInt range-checks the real part';

is "1+2i".Numeric, 1+2i, '.Numeric is the Complex';
is "42".Int, 42, 'a real string is untouched';
