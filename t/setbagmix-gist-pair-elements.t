use Test;

# A Set/Bag/Mix whose elements are Pairs (or other non-string typed values)
# renders each element via its original type's gist in `.gist` -- `a => 1`, not
# the internal `a\t1` string key. Plain Str/Int elements are unaffected.

plan 11;

# Pair elements keep their `key => value` gist
is set("a" => 1, "b" => 2).gist, 'Set(a => 1 b => 2)', 'Set of Pairs';
is bag("x" => 3).gist, 'Bag(x => 3)', 'Bag with a Pair element';
is mix("a" => 1.5).gist, 'Mix(a => 1.5)', 'Mix with a Pair element (fractional)';

# Plain elements are unchanged
is set("a", "b").gist, 'Set(a b)', 'Set of Str';
is set(1, 2, 3).gist, 'Set(1 2 3)', 'Set of Int';
is bag(1, 1, 2).gist, 'Bag(1(2) 2)', 'Bag of Int with weights';
is bag("x", "x", "x").gist, 'Bag(x(3))', 'single-key Bag';

# `(pair, ...).Mix` treats pairs as key=>weight (not Pair elements)
is (a => 1, b => 3).Mix.gist, 'Mix(a b(3))', 'pair-list .Mix uses key=>weight';

# The say/gist fast path and the explicit .gist method agree
is set("a" => 1).gist, set("a" => 1).gist, '.gist is deterministic';
{
    my $m = mix("k" => 2.5);
    is $m.gist, 'Mix(k => 2.5)', 'Mix Pair element via say-path helper';
}

# Empty containers unaffected
is set().gist, 'Set()', 'empty Set.gist';
