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

plan 27;

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

# --- a CONTAINER or reference-type value keeps OBJECT identity ------------
#
# rakudo makes this distinction deliberately (commit 5031dab3ac, pinned by
# roast/S02-types/pair.t's "Clone of Pair does not share .WHICH"): a value that
# can change under the pair cannot be summarised by a content digest, so the
# pair falls back to its own object identity.

{
    my $v = 100;
    my $p := foo => $v;
    is $p.WHICH.^name, 'ObjAt', 'a container-held value gives the pair ObjAt';
    my $clone := $p.clone;
    isnt $clone.WHICH, $p.WHICH, 'so a clone does not share the identity';
    $v = 200;
    isnt $clone.WHICH, $p.WHICH, '... before or after the container is written';
}

is (foo => [1, 2]).WHICH.^name, 'ObjAt', 'an Array value gives the pair ObjAt';
is (foo => (1, 2)).WHICH.^name, 'ObjAt', 'a List value, too';
is (foo => {a => 1}).WHICH.^name, 'ObjAt', 'a Hash value, too';

# ... while every value-identified value keeps the digest.
is (foo => 100).WHICH.^name, 'ValueObjAt', 'an Int value keeps ValueObjAt';
is (foo => Set.new(1)).WHICH.^name, 'ValueObjAt', 'a Set value is value-identified';
is (foo => Any).WHICH.^name, 'ValueObjAt', 'a type object is value-identified';

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
