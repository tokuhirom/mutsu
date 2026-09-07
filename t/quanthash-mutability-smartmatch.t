use v6;
use Test;

# `Set` and `SetHash` (and the Bag/Mix pairs) are SIBLINGS under `Any`, not a
# class and its mutable subclass, so a smartmatch between them is False in
# BOTH directions:
#
#     Set.new("a") ~~ SetHash   # False
#     SetHash.new("a") ~~ Set   # False
#
# mutsu spells the mutable/immutable distinction as a `bool` inside one
# `Value` variant. `value_type_name` reads it, so the exact-name comparison in
# `type_matches` is the whole rule — but a stale "SetHash/BagHash/MixHash are
# mutable variants sharing the same Value variants" bridge accepted the
# immutable -> mutable direction unconditionally. Removing it exposed the
# other half of the same mistake: the role lists that a QuantHash satisfies
# (`QuantHash` itself, and `Associative`) named only the three immutable
# spellings. Every expectation here was measured against rakudo 2026.07.

plan 35;

# --- the ticket's repro: neither direction matches ----------------------
nok Set.new("a") ~~ SetHash, 'Set value does not match SetHash';
nok Set           ~~ SetHash, 'Set type object does not match SetHash';
nok Bag.new("a") ~~ BagHash, 'Bag value does not match BagHash';
nok Bag           ~~ BagHash, 'Bag type object does not match BagHash';
nok Mix.new("a") ~~ MixHash, 'Mix value does not match MixHash';
nok Mix           ~~ MixHash, 'Mix type object does not match MixHash';

nok SetHash.new("a") ~~ Set, 'SetHash does not match Set (already correct)';
nok BagHash.new("a") ~~ Bag, 'BagHash does not match Bag (already correct)';
nok MixHash.new("a") ~~ Mix, 'MixHash does not match Mix (already correct)';

# The coercer spellings produce the immutable types, so they follow.
nok set("a")           ~~ SetHash, 'set() does not match SetHash';
nok bag("a")           ~~ BagHash, 'bag() does not match BagHash';
nok mix("a")           ~~ MixHash, 'mix() does not match MixHash';
nok (1, 2).Set         ~~ SetHash, '.Set does not match SetHash';

# --- each spelling still matches ITSELF ---------------------------------
ok Set.new("a")     ~~ Set,     'Set matches Set';
ok SetHash.new("a") ~~ SetHash, 'SetHash matches SetHash';
ok Bag.new("a")     ~~ Bag,     'Bag matches Bag';
ok BagHash.new("a") ~~ BagHash, 'BagHash matches BagHash';
ok Mix.new("a")     ~~ Mix,     'Mix matches Mix';
ok MixHash.new("a") ~~ MixHash, 'MixHash matches MixHash';

my %q is SetHash;
ok %q ~~ SetHash, 'a `is SetHash` container matches SetHash';
nok %q ~~ Set,    'a `is SetHash` container does not match Set';

# --- both spellings do the same ROLES ----------------------------------
ok Set.new("a")     ~~ Setty, 'Set does Setty';
ok SetHash.new("a") ~~ Setty, 'SetHash does Setty';
ok Bag.new("a")     ~~ Baggy, 'Bag does Baggy';
ok BagHash.new("a") ~~ Baggy, 'BagHash does Baggy';
ok Mix.new("a")     ~~ Mixy,  'Mix does Mixy';
ok MixHash.new("a") ~~ Mixy,  'MixHash does Mixy';
ok Set.new("a")     ~~ QuantHash, 'Set does QuantHash';
ok SetHash.new("a") ~~ QuantHash, 'SetHash does QuantHash';
ok Set.new("a")     ~~ Associative, 'Set does Associative';
ok SetHash.new("a") ~~ Associative, 'SetHash does Associative';

# --- the consumers that read the same answer ---------------------------
sub takes-sethash(SetHash $x) { "bound" }
is (try takes-sethash(Set.new("a"))) // 'refused', 'refused',
    'a SetHash parameter refuses a Set';

my $picked;
given Set.new("a") {
    when SetHash { $picked = 'sethash' };
    when Set     { $picked = 'set' };
    default      { $picked = 'other' }
}
is $picked, 'set', 'a `when` chain picks Set, not SetHash, for a Set';

multi sub assoc-or-any(Associative $x) { 'assoc' }
multi sub assoc-or-any(Any $x)         { 'any' }
is assoc-or-any(SetHash.new("a")), 'assoc', 'a SetHash prefers the Associative candidate';

sub takes-hash(%h) { 'ok' }
is (try takes-hash(SetHash.new("a"))) // 'refused', 'ok',
    'a `%h` parameter accepts a SetHash';
