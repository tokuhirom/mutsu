use Test;

# `unique` and `repeated` used to compare every candidate against every value
# already seen, making both O(n^2). `MongoDB::Fast`'s
# t/08-request-id-unique.rakutest dedupes 160_000 concurrently minted request
# ids and never finished under mutsu, while rakudo answered in ~0.5s.
#
# The duplicate lookup is hash-bucketed now, but the bucket only NARROWS the
# scan -- the identity predicate still decides -- so this file pins both
# halves: the answers are unchanged for every value kind (including the ones
# that get no bucket, and the cross-kind identity arms the bucketing must not
# break), and a large input completes at all.

plan 27;

# --- answers, per value kind ---------------------------------------------

is-deeply (1, 1, 2, 2, 3).unique.List, (1, 2, 3), 'unique on Int';
is-deeply (1, 1, 2, 2, 3).repeated.List, (1, 2), 'repeated on Int';
is-deeply <a b a c b>.unique.List, ('a', 'b', 'c'), 'unique on Str';
is-deeply <a b a c b>.repeated.List, ('a', 'b'), 'repeated on Str';
is-deeply (True, False, True).unique.List, (True, False), 'unique on Bool';
is-deeply (1e0, 1e0, 2e0).unique.List, (1e0, 2e0), 'unique on Num';

# A type-strict identity: 1, "1", True and 1e0 are four distinct values, so
# they must not share a bucket with each other.
is (1, "1", True, 1e0).unique.elems, 4, 'unique keeps distinct types apart';

# All NaN bit patterns are one equality class, but -0e0 and 0e0 are not.
is (NaN, NaN, NaN).unique.elems, 1, 'every NaN is the same value';
is (0e0, -0e0).unique.elems, 2, 'signed zeros stay distinct';

# Int and BigInt are compared numerically across representations, so a big
# value that fits neither must still dedupe against its own kind.
is (2**70, 2**70, 2**70 + 1).unique.elems, 2, 'unique on BigInt';
is (2**70, 2**70, 2**70 + 1).repeated.elems, 1, 'repeated on BigInt';
is (1, 2**70, 1, 2**70).unique.elems, 2, 'unique mixing Int and BigInt';

# Values with no cheap identity hash fall back to the full scan; they must
# still be compared against the hashable ones, never skipped.
is (Any, Any, Nil, Nil).unique.elems, 2, 'unique on type objects';
is ((1, 2), (1, 2)).unique.elems, 2, 'two Lists are distinct values';
# A Set is a value type: two separately built Sets with the same elements are
# one value, so this is unbucketed AND deduped.
is (Set.new(1, 2), Set.new(1, 2), Set.new(1, 3)).unique.elems, 2, 'unique on Set by content';
my @mixed = 1, Any, "1", (1, 2), 1, Any, "1";
is @mixed.unique.elems, 4, 'unique across hashable and unhashable kinds';

class UniqueScaleProbe { has $.x }
my $probe = UniqueScaleProbe.new(x => 1);
is ($probe, $probe, UniqueScaleProbe.new(x => 1)).unique.elems, 2,
        'unique on instances by identity';

# --- adverbs -------------------------------------------------------------

is-deeply (1, 2, 3, 4).unique(as => * % 2).List, (1, 2), 'unique :as';
is-deeply (1, 2, 3, 4).repeated(as => * % 2).List, (3, 4), 'repeated :as';
is-deeply (1, 2, 3, 4).unique(with => * == *).List, (1, 2, 3, 4), 'unique :with';
is-deeply (1, 1, 2).unique(with => * == *).List, (1, 2), 'unique :with dedupes';
is-deeply (1, 1, 2).repeated(with => * == *).List, (1,), 'repeated :with';

# --- shape: Array, Seq, Slip all take the same path ----------------------

my @array = 1, 1, 2;
is-deeply @array.unique.List, (1, 2), 'unique on Array';
is-deeply (1, 1, 2).Seq.unique.List, (1, 2), 'unique on Seq';
is-deeply (|(1, 1, 2),).unique.List, (1, 2), 'unique on flattened Slip';

# --- scale ---------------------------------------------------------------

# 20_000 distinct values is the quadratic loop's worst case (nothing ever
# matches, so every candidate scanned the whole history). It took ~33s before;
# with the index it is milliseconds. A generous bound keeps this from being a
# timing-flaky test while still failing outright if the quadratic scan returns.
my $n = 20_000;
my @ids = ^$n;
is @ids.unique.elems, $n, "unique over $n distinct values";
is @ids.repeated.elems, 0, "repeated over $n distinct values";
