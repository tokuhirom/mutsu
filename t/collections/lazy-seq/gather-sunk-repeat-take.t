use Test;

# A gather body is sunk: `EXPR xx N` runs EXPR N times, and a `take` inside it
# suspends the gather, so `xx *` stays lazy. `(1 xx *) Z ...` keeps the infinite
# repeat instead of currying into a WhateverCode.

plan 7;

my $n = 0;
my $s = gather do { $n++; take $n } xx *;
is $s[^3].List, (1, 2, 3), 'gather do { take } xx * is lazy';

my $count = *;
my $t = gather do { take 'x' } xx $count;
is $t.head(2).List, ('x', 'x'), 'the repeat count may be a variable holding *';

is (gather do { take 7 } xx 3).List, (7, 7, 7), 'a finite count repeats that often';

is ((1 xx *) Z <a b>).elems, 2, 'Z of an infinite repeat stops at the shorter side';
is ((1 xx *) Z[&infix:<~>] <a b c>).join(','), '1a,1b,1c', 'Z with a metaop operator';

my $m = 0;
my @z = (gather do { $m++; take $m } xx *) Z <a b c>;
is @z.elems, 3, 'gather xx * zipped with a finite list ends';
is @z.map(*.[0]).List, (1, 2, 3), 'the gathered values arrive in order';
