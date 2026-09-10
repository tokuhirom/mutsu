use Test;

# Raku's reduction one-arg rule is `[op]($x)` == `op($x)`, and every set
# operator declares a real one-argument candidate that COERCES:
#
#     multi sub infix:<(|)>(QuantHash:D \a) { a }
#     multi sub infix:<(|)>(Any        \a) { a.Set }
#
# mutsu used to hand the single operand back untouched. Worse, it first
# decomposed a Set/Bag/Mix into its pairs -- a QuantHash does NOT do Iterable
# in rakudo (`Set ~~ Iterable` is False), so it is one operand, not a list.
#
# Every expectation below was measured against raku v2026.07 first.

plan 45;

# --- A QuantHash is ONE operand, so non-set reductions see it whole. ---------

is ([~] Set.new("a")),      'a',   '[~] Set is the set stringified, not a fold over its pairs';
is ([+] bag(1, 1, 2)),      3,     '[+] Bag numifies the bag (total weight)';
is ([*] Set.new("a", "b")), 2,     '[*] Set numifies the set (elem count)';
is ([,] Set.new("a")).elems, 1,    '[,] Set yields a one-element list holding the Set';
isa-ok ([,] Set.new("a"))[0], Set, '... and that element is the Set itself';
is-deeply ([min] Set.new("a")), Set.new("a"), '[min] Set returns the Set unchanged';
is-deeply ([max] Set.new("a")), Set.new("a"), '[max] Set returns the Set unchanged';

# `~` follows the same one-arg rule on the string side, with Blob excepted.
is-deeply ([~] 5),   "5",   '[~] 5 is the Str "5", not the Int 5';
is-deeply ([~] "ab"), "ab", '[~] Str is that Str';
is-deeply ([~] (1, 2)), "12", '[~] over a real list still folds';
isa-ok ([~] Buf.new(1, 2)), Buf, '[~] Blob returns the blob unchanged';

# --- The set-level operators: QuantHash passes through, anything else .Set ---

for < (|) (&) (^) > -> $op {
    is-deeply (EVAL "[$op] 3"), Set.new(3), "[$op] 3 coerces to Set";
    is-deeply (EVAL "[$op] (a => 2).Bag"), (a => 2).Bag, "[$op] Bag passes the Bag through";
    isa-ok (EVAL "[$op] <a b>.SetHash"), SetHash, "[$op] SetHash stays mutable";
}

# --- `(-)` keeps the tower level but always yields the IMMUTABLE spelling ----

is-deeply ([(-)] 3), Set.new(3), '[(-)] 3 coerces to Set';
is-deeply ([(-)] (a => 2).Bag), (a => 2).Bag, '[(-)] Bag is that Bag';
isa-ok ([(-)] <a b>.SetHash), Set, '[(-)] SetHash demotes to an immutable Set';
is ([(-)] <a b>.SetHash).^name, 'Set', '... and is named Set, not SetHash';
isa-ok ([(-)] (a => 2).BagHash), Bag, '[(-)] BagHash demotes to Bag';
isa-ok ([(-)] (a => 1.5).MixHash), Mix, '[(-)] MixHash demotes to Mix';

# --- `(+)` is baggy: it promotes Set-level operands to Bag ------------------

is-deeply ([(+)] 3), (3 => 1).Bag, '[(+)] 3 coerces to Bag';
is-deeply ([(+)] Set.new("a")), ("a" => 1).Bag, '[(+)] Set promotes to Bag';
is-deeply ([(+)] (a => 2).Bag), (a => 2).Bag, '[(+)] Bag is that Bag';
isa-ok ([(+)] <a b>.SetHash), Bag, '[(+)] SetHash promotes to an immutable Bag';
isa-ok ([(+)] (a => 1.5).MixHash), Mix, '[(+)] MixHash demotes to Mix';

# --- `(.)` is baggy too, but PRESERVES mutability --------------------------

is-deeply ([(.)] 3), (3 => 1).Bag, '[(.)] 3 coerces to Bag';
is-deeply ([(.)] Set.new("a")), ("a" => 1).Bag, '[(.)] Set promotes to Bag';
isa-ok ([(.)] <a b>.SetHash), BagHash, '[(.)] SetHash promotes to a mutable BagHash';
isa-ok ([(.)] (a => 2).BagHash), BagHash, '[(.)] BagHash stays a BagHash';
isa-ok ([(.)] (a => 1.5).MixHash), MixHash, '[(.)] MixHash stays a MixHash';

# --- The scan form applies the same rule to its FIRST element ---------------

{
    my @scan = [\(|)] <a>, <a>;
    is @scan.elems, 2, 'the scan has one element per operand';
    isa-ok @scan[0], Set, 'the first scan element is [(|)]("a"), i.e. a Set';
    is-deeply @scan[0], Set.new("a"), '... with the right content';
    is-deeply @scan[1], Set.new("a"), 'the second element folds as before';
}
is-deeply ([\(+)] 3).list, (((3 => 1).Bag),), 'a one-operand scan coerces too';

# --- Multi-operand reductions are unchanged --------------------------------

is-deeply ([(|)] Set.new("a"), Set.new("b")), Set.new("a", "b"), 'two-operand union still folds';
is-deeply ([(+)] bag(1), bag(1)), (1 => 2).Bag, 'two-operand bag sum still folds';
is-deeply ([(&)] <a b>, <b c>), Set.new("b"), 'two-operand intersection still folds';
is-deeply ([(|)]), set(), 'the zero-operand identity is unchanged';
