use Test;
plan 4;

# `* OP= *` primes both stars: a two-argument WhateverCode (#13311).
my $x = 3;
is (* += *)($x, 5), 8, '* += * takes two arguments';
is $x, 8, 'the first argument container is written back';
is (* ~ *)("a", "b"), "ab", 'plain two-star curry unchanged';

my @a = 1, 2, 3;
@a.map(* += 1).eager;
is @a, [2, 3, 4], '@a.map(* += 1) writes back';
