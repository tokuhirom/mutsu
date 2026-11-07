use v6;
use Test;

# `sum(...)`, `min(...)` and `max(...)` of a `.map`/`.grep` Seq over a gather
# answered 0 / Inf / -Inf: the pipe's elements live in the gather's coroutine
# and the routine read an empty seed. The method forms and a plain array source
# were right. Found with Math::Sequences (`necklaces`). Identical under raku.

plan 8;

sub dv($n) { gather { take 1; take 2; take $n } }

is sum(dv(6).map({ $_ })), 9, 'sum of a map over a gather';
is sum(dv(6).grep({ True })), 9, 'sum of a grep over a gather';
is max(dv(6).map({ $_ * 2 })), 12, 'max of a map over a gather';
is min(dv(6).map({ $_ + 1 })), 2, 'min of a map over a gather';
is sum(dv(6).map: -> $d { $d * 2 }), 18, 'a pointy-block map';
is sum((1, 2, 6).map({ $_ })), 9, 'a map over a list (unchanged)';
is dv(6).map({ $_ }).sum, 9, 'the method form (unchanged)';
my $s = dv(6).map({ $_ });
is sum($s), 9, 'a Seq held in a variable';
