use v6;
use Test;

# A ternary then-branch may be a bare sigilless term (`my \foo = …`; then
# `cond ?? foo !! bar`). The parser guards a bare identifier in then-position as
# a possible listop head that gobbled the `!!`, but a sigilless term is a
# complete nullary term, not a listop head, so it must be accepted. Regression
# from Astro::Utils: `my \y0 = ye % 2 == 0 ?? ye !! ye - 1`.

plan 8;

# Sigilless term in the then-branch, various contexts.
{
    my \ye = 4;
    my \y0 = 1 ?? ye !! 9;
    is y0, 4, 'sigilless term in then-branch (my \ target)';
}
{
    my \ye = 4;
    my $r = 1 ?? ye !! 9;
    is $r, 4, 'sigilless term in then-branch (my $ target)';
}
{
    my \ye = 4;
    my \y0 = 0 ?? ye !! 9;
    is y0, 9, 'else-branch taken, then-branch still a valid term';
}

# The exact Astro::Utils shape: sub sigilless param used in a ternary then-arm.
{
    sub interp(\ye) {
        my \y0 = ye % 2 == 0 ?? ye !! ye - 1;   # even numbers only
        y0;
    }
    is interp(8), 8, 'even input keeps its value (then-branch)';
    is interp(7), 6, 'odd input drops to previous even (else-branch)';
}

# Bare ternary statement (no declaration target) with a term then-branch.
{
    my \ye = 5;
    my $out = do { 1 ?? ye !! 0 };
    is $out, 5, 'bare ternary with term then-branch in a do-block';
}

# Regression: a real listop head in then-position still binds its args, and a
# type object in then-position is still a complete then-expression.
{
    sub add2($a, $b) { $a + $b }
    is (1 ?? add2 2, 3 !! 99), 5, 'listop then-branch still consumes its args';
    is (1 ?? Int !! Str), Int, 'type object in then-branch still accepted';
}
