use Test;

plan 36;

# BagHash weights can exceed a native i64 (arbitrary-precision counts).
{
    my %h is BagHash;
    %h<foo> = 10000000000000000000;        # 10^19, > i64::MAX (~9.2e18)
    is %h<foo>, 10000000000000000000, 'BagHash weight can be set above i64::MAX';
    is %h<foo>.WHAT.^name, 'Int', 'big weight reads back as an Int';
}

# .total returns the full arbitrary-precision sum.
{
    my %h is BagHash;
    %h<a> = 10000000000000000000;
    %h<b> = 10000000000000000000;
    is %h.total, 20000000000000000000, '.total sums beyond i64::MAX';
}

# Bag coercion from a Hash literal preserves a big weight.
{
    is {'red' => 200000000000000000019}.Bag.<red>,
       200000000000000000019,
       'Hash.Bag preserves a weight larger than a native int';
}

# Bag coercion from pairs.
{
    my $b = (a => 10**20, b => 3).Bag;
    is $b<a>, 100000000000000000000, 'pair .Bag keeps the big weight';
    is $b<b>, 3, 'small co-resident weight is intact';
    is $b.total, 100000000000000000003, 'mixed-magnitude .total is exact';
}

# Ordinary small bags still behave.
{
    my $b = bag <a a b c c c>;
    is $b<a>, 2, 'small bag count a';
    is $b<c>, 3, 'small bag count c';
    is $b.total, 6, 'small bag total';
}

# Decrementing a big weight via assignment.
{
    my %h is BagHash;
    %h<x> = 10**19;
    %h<x> = %h<x> - 1;
    is %h<x>, 9999999999999999999, 'big weight decrement stays exact';
}

# Round-trip through Bag (immutable) coercion keeps precision.
{
    my %h is BagHash;
    %h<k> = 10**21;
    my $immutable = %h.Bag;
    is $immutable<k>, 1000000000000000000000, 'BagHash.Bag round-trips a huge weight';
}

# --- The baggy OPERATORS carry the same precision -------------------------
#
# Every one of them used to flatten `BagData.counts` (a BigInt map) down to
# i64 at coercion time, so `10**30` became `i64::MAX` and `(+)` then panicked
# one line later on "attempt to add with overflow" in a debug build (and wrapped
# silently in release). Expectations measured against raku v2026.07.

my $big = (a => 10**30).Bag;
my $one = (a => 1).Bag;

is ($big (+) $one)<a>, 10**30 + 1, '(+) adds beyond i64::MAX';
is ($big (-) $one)<a>, 10**30 - 1, '(-) subtracts beyond i64::MAX';
is ($big (&) $one)<a>, 1,          '(&) takes the min weight';
is ($big (|) $one)<a>, 10**30,     '(|) takes the max weight';
is ($big (^) $one)<a>, 10**30 - 1, '(^) is |a - b| at full precision';
is ($big (.) $one)<a>, 10**30,     '(.) multiplies at full precision';
is ((a => 10**30).Bag (.) (a => 10**10).Bag)<a>, 10**40,
   '(.) of two huge weights does not overflow';

# The mixed-operand paths take a different coercion route than Bag-vs-Bag.
is ($big (|) <a b>)<a>, 10**30, '(|) against a plain list keeps the big weight';
is (<a b> (|) $big)<a>, 10**30, '... in either operand position';
is ($big (+) <a>)<a>,   10**30 + 1, '(+) against a plain list';
is ($big (+) {a => 10**30})<a>, 2 * 10**30, '(+) against a Hash';
is ($big (+) (a => 10**30))<a>, 2 * 10**30, '(+) against a bare Pair';

# The mutable spelling and the reduction entry point use their own helpers.
is ((a => 10**30).BagHash (+) $one)<a>, 10**30 + 1, '(+) on a BagHash';
is ([(+)] $big, $big, $big)<a>, 3 * 10**30, 'the [(+)] reduction is exact';
is ([(-)] $big, $one)<a>, 10**30 - 1, 'the [(-)] reduction is exact';
is ([(.)] $big, (a => 2).Bag)<a>, 2 * 10**30, 'the [(.)] reduction is exact';

# Multi-key and multi-operand shapes.
{
    my $r = (a => 10**30, b => 5).Bag (^) (a => 1, b => 10**20).Bag;
    is $r<a>, 10**30 - 1, 'multi-key (^) first key';
    is $r<b>, 10**20 - 5, 'multi-key (^) second key';
}

# Set equality on huge weights must compare the real numbers, not saturated ones.
ok  ((a => 10**30).Bag (==) (a => 10**30).Bag), 'two equal huge bags are (==)';
nok ((a => 10**30).Bag (==) (a => 10**30 + 1).Bag),
    'two huge bags differing by 1 are not (==) (both used to saturate to i64::MAX)';

# Ordinary weights are unchanged, including the coercion corner cases.
is-deeply (bag(1, 1, 2) (+) bag(2)), (1 => 2, 2 => 2).Bag, 'small (+) is unchanged';
is-deeply ((a => 2.9).Bag), ("a" => 2).Bag, 'a fractional weight truncates toward zero';
is-deeply ((a => True).Bag), ("a" => 1).Bag, 'a Bool weight is 1';
is-deeply ((a => -5).Bag), bag(), 'a negative weight drops the element';
