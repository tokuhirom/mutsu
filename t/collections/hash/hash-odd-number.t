use Test;

plan 13;

# A single bare scalar (a one-element, odd initializer) assigned to a hash is
# X::Hash::Store::OddNumber.
throws-like 'my %h = 1', X::Hash::Store::OddNumber,
    'my %h = 1 is an odd hash initializer';
throws-like 'my %h = "key"', X::Hash::Store::OddNumber,
    'my %h = "key" is an odd hash initializer';
throws-like 'my %h = 1, 2, 3', X::Hash::Store::OddNumber,
    'an odd-length list is an odd hash initializer';

# A type object is one (odd) element too (#9774), and the message names the
# element the way rakudo does.
throws-like 'my %h = Any', X::Hash::Store::OddNumber,
    message => "Odd number of elements found where hash initializer expected:\nOnly saw: type object 'Any'",
    'my %h = Any is an odd hash initializer';
throws-like 'my %h; %h = Int', X::Hash::Store::OddNumber,
    message => "Odd number of elements found where hash initializer expected:\nOnly saw: type object 'Int'",
    'assigning a type object to a declared hash is too';
throws-like 'my %h = 1.5', X::Hash::Store::OddNumber,
    message => "Odd number of elements found where hash initializer expected:\nOnly saw: 1.5",
    'a single element shows as "Only saw"';
throws-like 'my %h = "a", "b", "c"', X::Hash::Store::OddNumber,
    message => "Odd number of elements found where hash initializer expected:\nFound 3 (implicit) elements:\nLast element seen: \"c\"",
    'a longer list shows its count and its last element';
{
    my %h = Any, 1;
    is %h.elems, 1, 'a type object with a value is still a key/value pair';
}

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
