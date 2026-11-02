use Test;

# From the CRDT ecosystem distribution (LWW-Hash): `%!add ∪= %add` on a
# `has %!add is SetHash` attribute. The meta-assignment writes INTO the
# QuantHash container, which keeps its SetHash/BagHash type; it used to become
# a plain Hash, so the next `%!add.set: ...` died with "No such method 'set'".

plan 7;

class C {
    has %!s is SetHash;
    has %!b is BagHash;
    method union(%x)   { %!s ∪= %x; %!s.WHAT.raku }
    method bag-add(%x) { %!b ⊎= %x; %!b.WHAT.raku }
    method set-key($k) { %!s.set: $k; %!s.keys.sort.join(',') }
    method plain(%x)   { %!s = %!s ∪ %x; %!s.WHAT.raku }
}

my %x = z => True;
is C.new.union(%x), 'SetHash', '∪= keeps the SetHash attribute type';
is C.new.bag-add(%x), 'BagHash', '⊎= keeps the BagHash attribute type';
is C.new.plain(%x), 'SetHash', 'the written-out form already did';

my $c = C.new;
$c.union(%x);
is $c.set-key('a'), 'a,z', 'SetHash.set works after the meta-assignment';
$c.union({ y => True });
is $c.set-key('b'), 'a,b,y,z', 'accumulates across several ∪=';

my %loc is SetHash;
%loc ∪= %x;
is %loc.WHAT.raku, 'SetHash', 'a local SetHash keeps its type too';

my %plain;
%plain ∪= %x;
is %plain.WHAT.raku, 'Hash', 'an ordinary Hash is still a Hash';
