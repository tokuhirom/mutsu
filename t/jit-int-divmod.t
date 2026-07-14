use v6;
use Test;

# `div` / `mod` are inlined by the JIT's Tier B (native sdiv/srem plus a floor
# correction). The loops below are hot enough to be JIT-compiled, so these pin
# the native path against the interpreter's `div_floor` / `mod_floor` semantics.

plan 9;

# Raku's `div`/`mod` floor toward negative infinity (unlike C's truncation),
# and `mod` takes the sign of the divisor.
is (7 div 3, -7 div 3, 7 div -3, -7 div -3).join(","), "2,-3,-3,2", 'div floors';
is (7 mod 3, -7 mod 3, 7 mod -3, -7 mod -3).join(","), "1,2,-2,-1", 'mod takes the divisor sign';

# Hot loop: the JIT compiles this, so the native path must agree with the
# interpreter on every sign combination.
{
    my @cases = (7, 3), (-7, 3), (7, -3), (-7, -3), (0, 5), (100, 7), (-100, 7), (5, 1), (1, -1);
    my $bad = 0;
    for ^200 {
        for @cases -> ($a, $b) {
            # The defining identity of floor division.
            $bad++ unless $a == $b * ($a div $b) + ($a mod $b);
            # `mod` lies in [0, b) for b > 0 and (b, 0] for b < 0.
            my $m = $a mod $b;
            $bad++ if $b > 0 && !(0 <= $m < $b);
            $bad++ if $b < 0 && !($b < $m <= 0);
        }
    }
    is $bad, 0, 'div/mod identity and range hold over a JIT-hot loop';
}

# Edge values around the small-Int boundary still round-trip.
is 0 div 5, 0, 'zero dividend';
is 0 mod 5, 0, 'zero dividend mod';

# A zero divisor must not be handled by the native path -- it falls to the
# interpreter, which produces a Failure rather than a wrong number.
{
    my $v = 1 div 0;
    ok $v ~~ Failure, 'div by zero yields a Failure';
    $v.so;  # mark handled
}
{
    my $v = 1 mod 0;
    ok $v ~~ Failure, 'mod by zero yields a Failure';
    $v.so;  # mark handled
}

# Values beyond the small-Int (48-bit) range fall to the interpreter's BigInt arm.
is (10 ** 20) div 3, 33333333333333333333, 'big dividend divs correctly';
is (10 ** 20) mod 7, (10 ** 20) - 7 * ((10 ** 20) div 7), 'big dividend mods consistently';
