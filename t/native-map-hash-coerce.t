use Test;

# `.Map` / `.Hash` coercion native dispatch (VM-native, shared impl with the
# interpreter via `builtins::map_hash_coerce`). `.Map` decontainerizes and
# embeds the `Map` declared-type in the resulting `Value::Hash` Arc (a pure
# value op since container metadata travels in the value, #2952). Behavior must
# match the interpreter / raku.

plan 21;

# --- .Hash from a flat list of pairs --------------------------------------
my $h = (a => 1, b => 2).Hash;
isa-ok $h, Hash, 'List-of-pairs.Hash is a Hash';
is $h<a>, 1, 'Hash value a';
is $h<b>, 2, 'Hash value b';

# --- .Hash from alternating key/value list --------------------------------
my $h2 = (<x 10 y 20>).Hash;
is $h2<x>, 10, 'alternating list Hash x';
is $h2<y>, 20, 'alternating list Hash y';

# --- .Hash from Set/Bag/Mix ----------------------------------------------
is <a b a>.Set.Hash<a>, True, 'Set.Hash membership';
is <a a b>.Bag.Hash<a>, 2,    'Bag.Hash weight';
is (a => 1.5, b => 2).Mix.Hash<a>, 1.5, 'Mix.Hash weight';

# --- .Map basics ----------------------------------------------------------
my $m = (a => 1, b => 2).Map;
isa-ok $m, Map, 'List-of-pairs.Map is a Map';
is $m.^name, 'Map', '.Map .^name is Map';
is $m<a>, 1, 'Map value a';
is $m.elems, 2, 'Map elems';

# --- .Map from a Hash decontainerizes & keeps identity on re-coerce -------
my %src = a => 1, b => 2;
my $m2 = %src.Map;
is $m2.^name, 'Map', '%h.Map is a Map (variable receiver)';
is $m2<b>, 2, 'Map from hash value';
ok $m2.Map === $m2, 'Map.Map is identity (same WHICH)';

# --- a Map is immutable ---------------------------------------------------
my $died = False;
{ $m2<c> = 9; CATCH { default { $died = True } } }
ok $died, 'assigning to a Map dies (immutable)';

# --- .Hash variable receiver ----------------------------------------------
my @pairs = (a => 1), (b => 2);
is @pairs.Hash.^name, 'Hash', '@a.Hash (variable receiver)';
is @pairs.Hash<a>, 1, '@a.Hash value';

# --- odd number of elements throws ---------------------------------------
my $odd = False;
{ my $bad = (1, 2, 3).Hash; CATCH { default { $odd = True } } }
ok $odd, 'odd element count throws X::Hash::Store::OddNumber';

# --- empty ----------------------------------------------------------------
is ().Hash.elems, 0, 'empty list .Hash is empty';
is ().Map.^name,  'Map', 'empty list .Map is a Map';
