use Test;

# A named `@` target of a pointy-block sub-signature must not consume a Seq
# value: the same hash is unpacked again by the next loop (Red t/41-fallback,
# `for @tests -> (:&code!, :@expected!, ...)` run by two subtests).

plan 3;

my @tests = %( :e((1, 2).map: * * 2) ),;
my @seen;
for 1..2 {
    for @tests -> (:@e!) { @seen.push(@e.elems) }
}
is-deeply @seen, [2, 2], 'a Seq bound to :@e twice keeps its elements';

my $s = (1, 2, 3).map: * * 2;
my $h = %( :e($s) );
my @sums;
for ($h, $h) -> (:@e!) { @sums.push(@e.sum) }
is-deeply @sums, [12, 12], 'the same hash unpacked twice in one loop';

my $explicit = (7, 8).map: * + 0;
$explicit.list;
throws-like { $explicit.list }, X::Seq::Consumed, 'an explicit .list still consumes a Seq';
