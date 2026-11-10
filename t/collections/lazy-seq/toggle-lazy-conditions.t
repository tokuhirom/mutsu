use Test;

plan 7;

# `*@conditions` flattens an iterable argument, lazily.
my constant $tc = (* mod 10 < 5) xx *;
is (^30).toggle($tc).List, (0, 1, 2, 3, 4, 10, 11, 12, 13, 14, 20, 21, 22, 23, 24),
    "toggle with an infinite xx * Seq of tests";
is (^30).toggle(:off, $tc).List, (0, 1, 2, 3, 4, 10, 11, 12, 13, 14, 20, 21, 22, 23, 24),
    "toggle(:off) with an infinite Seq of tests";

my @c = (* < 4), (* > 6);
is (1..10).toggle(@c).List, (1, 2, 3, 7, 8, 9, 10), "toggle with an Array of tests";
is (1..10).toggle(* < 4, * > 6).List, (1, 2, 3, 7, 8, 9, 10), "toggle with positional tests";
is (1..10).toggle(:off, * < 4, * > 6).List, (1,), "toggle(:off) with positional tests";
is (1..10).toggle(* < 4).List, (1, 2, 3), "toggle with one test";

# Pulling the tests must not be quadratic: 100000 items flip 20000 times.
my $t = now;
my $n = (^100000).toggle((* mod 10 < 5) xx *).elems;
is $n, 50000, "a long toggle over an infinite Seq of tests";
diag "took {now - $t}" if now - $t > 20;
