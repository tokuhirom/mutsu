use Test;

plan 8;

# A single bare scalar (a one-element, odd initializer) assigned to a hash is
# X::Hash::Store::OddNumber.
throws-like 'my %h = 1', X::Hash::Store::OddNumber,
    'my %h = 1 is an odd hash initializer';
throws-like 'my %h = "key"', X::Hash::Store::OddNumber,
    'my %h = "key" is an odd hash initializer';
throws-like 'my %h = 1, 2, 3', X::Hash::Store::OddNumber,
    'an odd-length list is an odd hash initializer';

# Even/valid initializers are unaffected.
{
    my %h = 1, 2;
    is %h{1}, 2, 'an even key/value list assigns';
}
{
    my %h = a => 1, b => 2;
    is %h<a> + %h<b>, 3, 'pair-list assignment works';
}
{
    my %o = x => 9; my %h = %o;
    is %h<x>, 9, 'hash-to-hash assignment copies';
}
{
    my $s = { y => 7 }; my %h = $s;
    is %h<y>, 7, 'a hash held in a scalar assigns';
}
{
    my %h = Nil;
    is %h.elems, 0, 'assigning Nil yields an empty hash';
}
