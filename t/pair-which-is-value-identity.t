use Test;

# A Pair's identity is value-based in raku: it is composed from the key's and
# the value's own `.WHICH`. mutsu's `.WHICH` METHOD had no Pair case, so a Pair
# fell to the per-object global-counter tail -- which made two structurally
# identical pairs differ, and made the string unstable even across two reads of
# the SAME pair.
#
# The internal keying (`runtime::utils::value_which_key`, what Set/Bag/Mix
# element keys already use) always had the right encoding; only the method
# disagreed. That is why `===`, `eqv` and object-hash keys were already correct
# and are asserted here as the thing that must NOT move.
#
# Every expectation below was measured against raku v2026.07 first. The one
# thing deliberately not asserted is the exact spelling: rakudo renders a Pair's
# identity as an opaque digest (`Pair|58DC55B1...`), which nothing can pin.

plan 18;

ok  (a => 1).WHICH eq (a => 1).WHICH, 'two structurally identical pairs share an identity';
ok  (a => 1).WHICH eq (:a(1)).WHICH, 'the adverbial spelling is the same pair';
ok  (a => 1).WHICH eq ("a" => 1).WHICH, 'a quoted key is the same pair';
nok (a => 1).WHICH eq (a => 2).WHICH, 'a different value is a different pair';
nok (a => 1).WHICH eq (b => 1).WHICH, 'a different key is a different pair';

{
    my $p = (a => 1);
    ok $p.WHICH eq $p.WHICH, 'the identity of one pair is stable across reads';
}

# A Pair is value-identified, so its `.WHICH` is a ValueObjAt.
is (a => 1).WHICH.^name, 'ValueObjAt', '.WHICH on a Pair is a ValueObjAt';

# Non-Str keys and nested pairs recurse through the same encoding.
ok (1 => 2).WHICH eq (1 => 2).WHICH, 'an Int-keyed pair';
nok (1 => 2).WHICH eq ("1" => 2).WHICH, 'an Int key is not a Str key';
ok (a => (b => 2)).WHICH eq (a => (b => 2)).WHICH, 'a nested pair';
nok (a => (b => 2)).WHICH eq (a => (b => 3)).WHICH, '... distinguished by its inner value';

# --- what must NOT move: these were already correct -----------------------

ok  (a => 1) === (a => 1), '=== on identical pairs';
nok (a => 1) === (a => 2), '=== on differing pairs';
ok  (a => 1) eqv (a => 1), 'eqv on identical pairs';

{
    my %h{Any};
    %h{(a => 1)} = 'v';
    is %h{(a => 1)}, 'v', 'an object hash keyed by a pair reads back';
    is %h.elems, 1, '... with one entry';
    %h{(a => 2)} = 'w';
    is %h.elems, 2, 'a differing pair is a distinct key';
}

is Set.new((a => 1), (a => 1)).elems, 1, 'a Set collapses two identical pairs';
