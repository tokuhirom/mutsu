use Test;

# Pin for the VM-native QuantHash / Map / Hash coercion of *plain Cool scalar*
# receivers (ledger §D). `"a".Set` / `42.Bag` / `$s.Mix` etc. used to fall back to
# the interpreter because the native gate only accepted list-like aggregates; the
# scalar now folds to a single-element collection via the same shared
# `builtins::quanthash_coerce` / `builtins::map_hash_coerce` impl the interpreter
# uses, so the result is byte-identical. Both literal receivers (non-mut path) and
# variable receivers (`$s.Set` -> CallMethodMut, mut path) are exercised.

plan 39;

sub showset($s) { $s.keys.sort.join(' ') }

# --- .Set on scalars (literal = non-mut path) ---
isa-ok "blue".Set, Set, 'Str.Set is a Set';
is showset("blue".Set), 'blue', 'Str.Set has the string element';
isa-ok 42.Set, Set, 'Int.Set is a Set';
is showset(42.Set), '42', 'Int.Set has the int element';
isa-ok (1/2).Set, Set, 'Rat.Set is a Set';
is showset((1/2).Set), '0.5', 'Rat.Set has the rat element';
isa-ok True.Set, Set, 'Bool.Set is a Set';

# Int keys keep their type (set membership by value, not just string)
ok 42.Set{42}, 'Int.Set membership tests true for the Int key';
nok 42.Set{43}, 'Int.Set membership tests false for a different key';

# --- .Bag / .Mix on scalars ---
isa-ok "a".Bag, Bag, 'Str.Bag is a Bag';
is "a".Bag<a>, 1, 'Str.Bag has weight 1';
isa-ok 7.Bag, Bag, 'Int.Bag is a Bag';
is 7.Bag{7}, 1, 'Int.Bag has weight 1 for the Int key';
isa-ok 3.Mix, Mix, 'Int.Mix is a Mix';
is 3.Mix{3}, 1, 'Int.Mix has weight 1';
isa-ok "z".Mix, Mix, 'Str.Mix is a Mix';

# --- *Hash variants are mutable ---
isa-ok "a".SetHash, SetHash, 'Str.SetHash is a SetHash';
isa-ok "a".BagHash, BagHash, 'Str.BagHash is a BagHash';
isa-ok "a".MixHash, MixHash, 'Str.MixHash is a MixHash';
my $sh = "a".SetHash;
$sh{"b"} = True;
is showset($sh), 'a b', 'SetHash from a scalar is mutable';

# --- variable receivers (mut path / CallMethodMut) ---
my $s = "cat";
isa-ok $s.Set, Set, 'variable Str.Set is a Set';
is showset($s.Set), 'cat', 'variable Str.Set has the string element';
my $n = 99;
is showset($n.Bag), '99', 'variable Int.Bag has the int element';
my $r = 2/4;
is showset($r.Mix), '0.5', 'variable Rat.Mix has the rat element';
my $b = False;
isa-ok $b.SetHash, SetHash, 'variable Bool.SetHash is a SetHash';

# Receiver is not mutated by the coercion (no writeback)
my $orig = "keep";
$orig.Set;
is $orig, 'keep', 'coercion does not mutate the scalar receiver';

# --- .Map / .Hash on scalars ---
# A single Pair-less scalar is an odd number of elements -> dies, same as raku.
dies-ok { "a".Hash }, 'Str.Hash on a lone scalar dies (odd number)';
dies-ok { 42.Hash }, 'Int.Hash on a lone scalar dies (odd number)';
dies-ok { "a".Map }, 'Str.Map on a lone scalar dies (odd number)';
my $k = "lonely";
dies-ok { $k.Hash }, 'variable Str.Hash on a lone scalar dies (odd number)';

# --- aggregate receivers still behave (unchanged path) ---
isa-ok <a b c a>.Set, Set, 'list .Set still a Set';
is showset(<a b c a>.Set), 'a b c', 'list .Set dedups';
is showset([1, 2, 2, 3].Bag), '1 2 3', 'array .Bag keys';
isa-ok (1, 2, 3).Mix, Mix, 'list .Mix still a Mix';
my %h = a => 2, b => 0;
is showset(%h.Set), 'a', 'hash .Set keeps truthy-weight keys';
my @a = <x y x>;
is showset(@a.Bag), 'x y', 'array variable .Bag keys';
is %h.Hash<a>, 2, 'hash .Hash round-trips';
isa-ok (a => 1, b => 2).Map, Map, 'list-of-Pairs .Map is a Map';
is (a => 1, b => 2).Map<b>, 2, 'list-of-Pairs .Map round-trips a value';
