use Test;

plan 5;

# Test that classes with native-typed attributes can be constructed efficiently.
# This is a regression test for a performance bug where native type defaults
# (e.g., `has uint32 $.a` generating `default: Int(0)`) triggered expensive
# env manipulation on every construction, causing ~800us per attribute.

my class Data {
    has uint32 $.a;
    has uint32 $.b;
    has uint32 $.c;
    has uint32 $.hashlen;
}

# Basic construction with no args
{
    my $d = Data.new;
    is $d.a, 0, 'uint32 attr defaults to 0';
    is $d.hashlen, 0, 'uint32 attr defaults to 0 (2)';
}

# Construction with named args
{
    my $d = Data.new(:a(42), :hashlen(32));
    is $d.a, 42, 'uint32 attr set via named arg';
    is $d.hashlen, 32, 'uint32 attr set via named arg (2)';
}

# Loop construction (should complete in reasonable time)
{
    my $count = 0;
    for 1..1000 {
        my $d = Data.new(:hashlen(32));
        $count++ if $d.hashlen == 32;
    }
    is $count, 1000, 'constructed 1000 instances with native-typed attrs';
}
