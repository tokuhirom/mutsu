use Test;

# `.polymod` must reify a LAZY divisor source (a `gather` block, `.map`, or a
# `… ∞` sequence) before dividing. Previously an unforced finite lazy source (a
# plain `gather {...}`) had an empty element cache, so none of its divisors were
# applied and the number fell through unchanged: `600.polymod(gather {...})`
# wrongly returned `(600)` instead of `(0 2 6 3)`.

plan 8;

is-deeply 600.polymod(gather { take 3 * $_ for 1..3 }).List, (0, 2, 6, 3),
    'finite gather divisor list is reified';

is-deeply 600.polymod(lazy gather { take 3 * $_ for 1..3 }).List, (0, 2, 6, 3),
    'lazy-prefixed gather divisor list is reified';

my $s = lazy gather { take 3 * $_ for 1..3 };
is-deeply 600.polymod($s).List, (0, 2, 6, 3),
    'gather Seq bound to a scalar is reified';

is-deeply 600.polymod((1..3).map(* * 3)).List, (0, 2, 6, 3),
    'a .map Seq divisor list is reified';

# Eager and lazy List divisors were already fine — guard against regression.
is-deeply 600.polymod(3, 6, 9).List, (0, 2, 6, 3), 'eager divisor list still works';
is-deeply 600.polymod(lazy 3, 6, 9).List, (0, 2, 6, 3), 'lazy List divisor still works';

# An infinite sequence divisor keeps its cached prefix; polymod pulls until n == 0.
is-deeply 120.polymod((10, 100 … ∞)).List, (0, 12), 'infinite sequence divisor still works';

# Plain scalar divisors are unaffected.
is-deeply 1000.polymod(10, 10, 10).List, (0, 0, 0, 1), 'plain scalar divisors still work';
