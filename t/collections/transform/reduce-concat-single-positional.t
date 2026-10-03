use Test;

plan 7;

# `[~] x` is `infix:<~>(x)`. Its `(@args)` candidate takes a lone Positional,
# even an itemized one, and joins it; anything else is stringified.
my $list = (1, 2);
is ([~] $list), '12', 'an itemized List is joined';
is ([~] $[1, 2]), '12', 'an itemized Array is joined';
my @m = [1, 2], [3, 4];
is ([~] @m[0]), '12', 'an array element holding an Array';
is ([~] $[]).raku, '""', 'an empty itemized Array is the empty string';
is ([~] $[[1, 2], 3]), '1 23', 'only the outer level is joined';
is ([~] 5).raku, '"5"', 'a lone non-Positional is stringified';
my @chunks = Blob.new(1);
isa-ok ([~] @chunks), Blob, 'a lone Blob element stays a Blob';
