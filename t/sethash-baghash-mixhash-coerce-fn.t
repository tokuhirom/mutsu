use Test;

# The mutable QuantHash coercion *functions* SetHash(...) / BagHash(...) /
# MixHash(...) (as opposed to the .SetHash/.BagHash/.MixHash methods or the
# immutable Set/Bag/Mix functions). These previously died with
# "Unknown function: SetHash".

plan 16;

# Basic coercion produces the mutable variant.
isa-ok SetHash(<a b c>), SetHash, 'SetHash(...) returns a SetHash';
isa-ok BagHash(<a b c>), BagHash, 'BagHash(...) returns a BagHash';
isa-ok MixHash(<a b c>), MixHash, 'MixHash(...) returns a MixHash';

# Element membership / weights match the immutable counterparts.
is SetHash(<a a b>).elems, 2, 'SetHash dedups elements';
is BagHash(<a a b>){'a'}, 2, 'BagHash counts occurrences';
is MixHash(<a a b>){'a'}, 2, 'MixHash sums weights';

# A single scalar arg coerces, then .Hash round-trips the object.
is SetHash(42).Hash.keys[0], 42, 'SetHash(42).Hash.keys[0] is the original Int';
isa-ok SetHash(42).Hash.keys[0], Int, 'SetHash(42).Hash key retains Int type';

# The result is mutable (unlike Set/Bag/Mix).
my $sh = SetHash(<a b>);
$sh<c> = True;
ok $sh<c>, 'SetHash(...) result is mutable (can add a key)';
is $sh.elems, 3, 'SetHash gained the new element';

my $bh = BagHash(<a a>);
$bh<a>++;
is $bh<a>, 3, 'BagHash(...) result is mutable (can bump a weight)';

my $mh = MixHash(<a>);
$mh<b> = 2.5;
is $mh<b>, 2.5, 'MixHash(...) result is mutable (can set a weight)';

# No-arg form stays a type object (does not become an empty container).
ok SetHash ~~ SetHash, 'bare SetHash is still the type object';
ok BagHash ~~ BagHash, 'bare BagHash is still the type object';
ok MixHash ~~ MixHash, 'bare MixHash is still the type object';

# Coercing from a Hash decomposes pairs (same as Set/Bag/Mix).
is SetHash({:a, :b}).elems, 2, 'SetHash(Hash) decomposes truthy pairs';
