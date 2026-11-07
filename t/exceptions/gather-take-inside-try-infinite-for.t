use Test;

# From Math::Sequences (A002904, t/numberphile): a `take` inside a `try`
# body, in a `for` over an infinite range, must suspend the lazy gather at
# the take. The `try` body's callframe used to count as a nested call, so the
# suspension was deferred forever and the pull never returned.

plan 6;

my @a = lazy gather for 1..* -> $n {
    try {
        take $n * 2;
        CATCH { when /'bad'/ { take 0 } }
    }
}
is @a[^5].List, (2, 4, 6, 8, 10), 'try + CATCH inside for 1..* in a lazy gather';

my $g = gather for 1..* { try { take $_ } };
is $g[^3].List, (1, 2, 3), 'plain try body';
is $g.head(2).List, (1, 2), 'head on the same gather';

my $h = gather for 1..* -> $n { try { take $n; take -$n } };
is $h[^5].List, (1, -1, 2, -2, 3), 'two takes inside the try';

my $c = gather for 1..* -> $n { try { die "bad" if $n %% 2; take $n; CATCH { default { take 0 } } } };
is $c[^6].List, (1, 0, 3, 0, 5, 0), 'a CATCH that takes after a failure';

my $nested = gather for 1..* -> $i { for 1..2 -> $j { try { take $i * 10 + $j } } };
is $nested[^5].List, (11, 12, 21, 22, 31), 'try in a nested for';
