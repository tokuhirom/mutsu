use Test;

# Pin for VM-native QuantHash coercion dispatch (ledger §1: native receiver
# dispatch -> VM-native). The element-folding logic is the single authoritative
# pure implementation in src/builtins/quanthash_coerce.rs, shared by both the
# bytecode VM (try_native_quanthash_coerce) and the interpreter fallback, so
# these results must match exactly.

plan 26;

# --- .Set ---
is (1, 2, 2, 3).Set.elems, 3, 'List.Set dedups';
is (1, 2, 2, 3).Set.sort, (1, 2, 3), 'Set membership';
ok <a b c> (<) <a b c d>.Set, 'Set subset op after coercion';
is (1, [2, 3]).Set.elems, 3, 'List flattens in .Set';
is [1, [2, 3]].Set.elems, 2, 'Array takes elements whole in .Set';
is (1..5).Set.elems, 5, 'Range.Set';
is %(a => 1, b => 0).Set.keys.sort, ('a',), 'Hash.Set drops false values';

# --- .Bag (weights) ---
is <a b b c>.Bag<b>, 2, 'List.Bag counts occurrences';
is <a b b c>.Bag.total, 4, 'Bag total';
is ('x' => 3, 'y' => 2).Bag<x>, 3, 'Pair weights in .Bag';
is ('x' => 0, 'y' => 2).Bag.keys.sort, ('y',), 'Bag drops zero-weight pairs';
is (1, [2, 2]).Bag.total, 3, 'List flattens in .Bag';
is [1, [2, 2]].Bag.elems, 2, 'Array whole in .Bag';

# --- .Mix (Real weights) ---
is (1.5, 2.5, 2.5).Mix<2.5>, 2, 'List.Mix counts occurrences';
is ('a' => 1.5, 'b' => 2.5).Mix<b>, 2.5, 'Mix keeps fractional weight';
is ('a' => 1.5, 'a' => 0.5).Mix<a>, 2.0, 'Mix sums duplicate keys';

# --- .SetHash / .BagHash (mutable) ---
{
    my $s = (1, 2, 3).SetHash;
    ok $s ~~ SetHash, '.SetHash is a SetHash';
    $s<4> = True;
    is $s.elems, 4, 'SetHash is mutable';
    $s<1>:delete;
    is $s.elems, 3, 'SetHash delete';
}
{
    my $b = <x y>.BagHash;
    ok $b ~~ BagHash, '.BagHash is a BagHash';
    $b<x>++;
    is $b<x>, 2, 'BagHash is mutable';
}

# --- .MixHash (interpreter-owned metadata path, must still work) ---
{
    my $m = (1, 2, 3).MixHash;
    ok $m ~~ MixHash, '.MixHash is a MixHash';
    $m<2> += 1.5;
    is $m<2>, 2.5, 'MixHash mutable fractional';
}

# --- coercions compose with set operators (the breadth driver) ---
{
    my $a = (1, 2, 3).Set;
    my $b = (2, 3, 4).Set;
    is ($a (&) $b).elems, 2, 'Set intersection of coerced sets';
    my $c = (1, 2).Set;
    my $d = (2, 3).Set;
    is ($c (|) $d).elems, 3, 'Set union of coerced sets';
}

# --- empty ---
is ().Set.elems, 0, 'empty List.Set';
