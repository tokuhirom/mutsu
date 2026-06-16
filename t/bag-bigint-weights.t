use Test;

plan 12;

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
