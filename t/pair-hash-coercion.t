use v6;
use Test;

# A bare `Pair` receiver coerces to a one-entry Hash/Map. mutsu used to reject
# every one of these with "Odd number of elements found where hash initializer
# expected", because the coercion's fallback arm matched only the *named-argument*
# pair flavour (`ValueView::Pair`) while ADR-0021 makes a literal mint the *data*
# flavour (`ValueView::ValuePair`). Every expectation below is `raku` v2026.07's.

plan 14;

# --- .Hash on a bare Pair -------------------------------------------------
is (a => 1).Hash.raku, '{:a(1)}', 'a bare Pair coerces to a one-entry Hash';
is (a => 1).Hash<a>, 1, '...and the entry is readable by key';
is (a => 1).hash.raku, '{:a(1)}', 'lowercase .hash agrees';
is (a => 1).Map.raku, 'Map.new((:a(1)))', '.Map agrees';

# --- non-Str keys stringify ----------------------------------------------
is (1 => "a").Hash.raku, '{"1" => "a"}', 'an Int key stringifies';
is ("x y" => 1).Hash.raku, '{"x y" => 1}', 'a non-identifier Str key is quoted';
{
    my $k = <a b>;
    is ($k => 1).Hash.raku, '{"a b" => 1}', 'a List key stringifies to its .Str';
}

# --- the value is stored as a Hash value, i.e. itemized (ADR-0040) --------
is (a => (1, 2)).Hash.raku, '{:a($(1, 2))}', 'a List value is itemized by the store';
is (a => [1, 2]).Hash.raku, '{:a($[1, 2])}', 'an Array value likewise';
{
    sub takes(*@a) { @a.elems }
    is takes((a => (1, 2)).Hash<a>), 1, '...so the value reads back as one item';
}
# ...but a Map's values are NOT containers, so .Map deconts again.
is (a => (1, 2)).Map.raku, 'Map.new((:a((1, 2))))', '.Map hands the value out bare';

# --- the shapes that already worked must not move -------------------------
is ((a => 1),).Hash.raku, '{:a(1)}', 'a one-element list of Pairs still works';
is ((a => 1), (b => 2)).Hash.raku, '{:a(1), :b(2)}', 'a two-element list of Pairs too';
dies-ok { ("a", "b", "c").Hash }, 'a genuinely odd item list still dies';

done-testing;
