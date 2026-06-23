use Test;

# `.map` on a concrete array returns a Seq (matches Rakudo), even when the
# block is a plain closure that takes the native fast path. The rw writeback of
# a topic-mutating block still updates the source array.
plan 8;

is [1, 2, 3].map(* + 1).^name, 'Seq', 'map with WhateverCode is a Seq';
is [1, 2, 3].map({ $_ * 2 }).^name, 'Seq', 'map with a block is a Seq';
is [1, 2, 3].map(-> $x { $x + 1 }).^name, 'Seq', 'map with a pointy block is a Seq';

is [1, 2, 3].map({ $_ * 2 }).raku, '(2, 4, 6).Seq', 'map.raku renders as a Seq';

my @a = 1, 2, 3;
is @a.map(* + 10).^name, 'Seq', 'map on a named array is a Seq';

# Multi-arity block still returns a Seq.
is [1, 2, 3, 4].map(-> $a, $b { $a + $b }).raku, '(3, 7).Seq', 'multi-arity map is a Seq';

# rw writeback: a topic-mutating block still updates the source array, and the
# returned value is still a Seq.
my @b = 1, 2, 3;
my $r = @b.map({ $_++ });
is $r.^name, 'Seq', 'topic-mutating map returns a Seq';
is @b.raku, '[2, 3, 4]', 'topic-mutating map still writes back to the array';
