use Test;

# A sigilless `my \x` is scoped to its block: once the block ends, a later,
# unrelated `my $x` (which shares the `x` local key) must still be an
# ordinary itemizing Scalar (#11228).

plan 6;

{ my \x = 1; }
{
    my $x = [1, 2];
    is $x.raku, '$[1, 2]', 'a `my $x` after an exited `my \x` block itemizes an Array';
    is $x.elems, 2, 'the itemized Array keeps its elements';
}

my %h = a => 1;
{ my \x = %h; }
{
    my $x = %h;
    is $x.raku, '${:a(1)}', 'a `my $x` after an exited `my \x` block itemizes a Hash';
}

{
    my \y = [3, 4];
    is y.raku, '[3, 4]', 'the sigilless binding itself is still not itemized';
}

{ my \str = 5; is str, 5, 'a sigilless `str` shadows the native type in its block' }
my str $s = "a";
is $s, "a", 'the native type is visible again after the block';
