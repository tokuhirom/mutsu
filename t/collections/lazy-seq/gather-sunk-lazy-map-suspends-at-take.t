use Test;
plan 11;

# A bare `.map` statement in a lazy gather is sunk element by element: every
# `take` in its callback is an element of the gather, and the gather suspends
# between pulls even when the mapped list is infinite.
my @g = lazy gather { (1..*).map: { take $_ } };
is-deeply @g[^3], (1, 2, 3), "infinite source: first pull";
is-deeply @g[3..6], (4, 5, 6, 7), "infinite source: a later pull resumes the pipe";

my @bells = lazy gather {
    my @src = lazy [1], -> @b { my @c = @b.tail; @c.push: @b[$_] + @c[$_] for ^@b; @c } ... *;
    @src.map: { take .head };
};
is-deeply @bells[^8], (1, 1, 2, 5, 15, 52, 203, 877), "Bell numbers from a self-feeding sequence";

my @h = lazy gather { (1..10).grep({ $_ %% 2 }).map: { take $_ * 10 }; take 99 };
is-deeply @h[^3], (20, 40, 60), "grep then map chain";

my @u = lazy gather { take 0; (1..3).map: { take $_ }; take 100 };
is-deeply @u[^5], (0, 1, 2, 3, 100), "statements around a finite sunk map still run";

my $side = 0;
my @s = lazy gather { (1..*).map: { $side++; take $_ } };
my $first = @s[0];
is $side, 1, "the callback runs only as far as the pull needs";

my @w = lazy gather { for 1..2 -> $i { (1..2).map: { take $i * 10 + $_ } } };
is-deeply @w[^4], (11, 12, 21, 22), "a sunk map inside a loop of the gather body";

my @e = gather { (1..3).map: { take $_ * 2 } };
is-deeply @e, [2, 4, 6], "an eager gather is unchanged";

# The tail of a `try` body is the statement's value and is sunk the same way.
my @t = lazy gather { try { (1..*).map: { take $_ * 2 } } };
is-deeply @t[^4], (2, 4, 6, 8), "infinite map as the tail of a try body";
is-deeply @t[4..5], (10, 12), "a later pull resumes it";
my @l = lazy gather { for 1..2 { try { (1..2).map: { take $_ } } } };
is-deeply @l[^4], (1, 2, 1, 2), "a try-tail map inside a loop";
