use Test;

# `.raku`/`.perl` of a Set/Bag/Mix (and the mutable *Hash variants) renders the
# proper constructor form -- `Set.new(1,2,3)`, `("a"=>2).Bag`, `(:x(1.5)=>1).Mix`
# -- with each element shown in its ORIGINAL type, and round-trips through EVAL.
# (Set/Bag/Mix iteration order is unspecified, so assert on round-trip and on
# the constructor shape rather than an exact string.)

plan 18;

# Round-trips (the authoritative correctness check)
{
    my $s = set(1, 2, 3);
    ok $s.raku.EVAL eqv $s, 'Set.raku round-trips';
}
{
    my $s = set("a", "b", "c");
    ok $s.raku.EVAL eqv $s, 'Set of Str round-trips';
}
{
    my $b = bag("a", "a", "b");
    ok $b.raku.EVAL eqv $b, 'Bag.raku round-trips';
}
{
    my $m = mix("x" => 1.5, "y" => 2);
    ok $m.raku.EVAL eqv $m, 'Mix.raku round-trips';
}
{
    my $sh = SetHash.new(1, 2, 3);
    ok $sh.raku.EVAL eqv $sh, 'SetHash.raku round-trips';
}
{
    my $bh = BagHash.new(1, 1, 2);
    ok $bh.raku.EVAL eqv $bh, 'BagHash.raku round-trips';
}

# Constructor shape
ok set(1, 2, 3).raku.starts-with('Set.new('), 'Set uses Set.new(...)';
ok set("a").raku.contains('"a"'), 'Str element is quoted in Set.raku';
ok set(7).raku.contains('7') && !set(7).raku.contains('"7"'),
    'Int element is unquoted in Set.raku';
ok bag("a", "a").raku.ends-with('.Bag'), 'Bag uses (...).Bag';
ok bag("a", "a").raku.contains('=>2'), 'Bag shows the weight';
ok mix("x" => 1.5).raku.ends-with('.Mix'), 'Mix uses (...).Mix';

# Empty immutable containers use the lowercase coercer form
is set().raku, 'set()', 'empty Set.raku is set()';
is bag().raku, 'bag()', 'empty Bag.raku is bag()';
is mix().raku, 'mix()', 'empty Mix.raku is mix()';

# A Set element inside an array keeps its constructor form in .raku
{
    my @a = [set(1, 2, 3)];
    is @a.raku.EVAL.elems, 1, 'array-with-Set .raku round-trips to 1 element';
    ok @a.raku.contains('Set.new('), 'Set element in array .raku keeps Set.new(...)';
}

# .raku of a Set as a Hash value (already worked) stays correct
{
    my %h = k => set(1, 2);
    ok %h.raku.contains('Set.new('), 'Set as a Hash value keeps Set.new(...)';
}
