use Test;

# Rakudo's `Num % Real` is `$a - floor($a / $b) * $b`: the rounding of the
# quotient and the product shows, and an infinite divisor gives NaN (#14195).

plan 16;

is 1e0 % 0.3, 0.10000000000000009, "1e0 % 0.3";
is 1e0 % 0.1e0, 0, "1e0 % 0.1e0 is exactly zero";
is 0.7 % 0.2e0, 0.09999999999999987, "0.7 % 0.2e0";
is 1.01e0 % 0.2, 1.01e0 - (1.01e0 / 0.2).floor * 0.2, "1.01e0 % 0.2 follows the formula";
is 5.5e0 % 2.5e0, 0.5, "5.5e0 % 2.5e0";
is 1e300 % 7e0, 0, "a huge dividend";
is 7e0 % -3e0, -2, "the result has the divisor's sign";
is -7e0 % 3e0, 2, "...for a negative dividend too";
is 7e0 % 3, 1, "a Num dividend and an Int divisor";
is -7 % 2.5e0, 0.5, "an Int dividend and a Num divisor";

is (3e0 % Inf).raku, NaN.raku, "an infinite divisor is NaN";
is (-3e0 % Inf).raku, NaN.raku, "...a negative dividend too";
is (3e0 % -Inf).raku, NaN.raku, "...and a negative infinite divisor";
is (Inf % 3e0).raku, NaN.raku, "an infinite dividend is NaN";
is (NaN % 3e0).raku, NaN.raku, "NaN stays NaN";
isa-ok 3e0 % 0e0, Failure, "a zero divisor is still the divide-by-zero Failure";
