use Test;

# Rakudo declares the quantity-hash and map coercers on Map, Seq, Set, Bag, Mix
# and Pair themselves; mutsu's rows sit on Any, which those classes reach
# through their MRO. `.^methods` lists them for each such class (#14134).

my @coercers = <Set SetHash Bag BagHash Mix MixHash Map Hash>;

for Map, Seq, Set, Bag, Mix -> \t {
    my $names = t.^methods.map(*.name).Set;
    # Seq declares the quantity-hash coercers but not Map/Hash.
    my @want = t ~~ Seq ?? @coercers.head(6) !! @coercers;
    my @missing = @want.grep({ !$names{$_} });
    is @missing.join(" "), "", "{t.^name}.^methods lists every coercer";
}

for Int, Str, Pair {
    my $names = .^methods.map(*.name).Set;
    nok $names<Bag>, "{.^name}.^methods does not list Bag (Rakudo declares none)";
}

is Map.new((a => 1)).Bag.raku, ("a" => 1).Bag.raku, "Map.Bag still answers";
is Map.new((a => 1)).Hash.raku, {:a(1)}.raku, "Map.Hash still answers";
ok Map.^find_method('Set').defined, "Map.^find_method('Set') finds it";
ok Map.^can('Mix').elems, "Map.^can('Mix')";
is Hash.^methods.map(*.name).grep('Bag').elems, 1, "Hash lists Bag once";
is Map.^methods.map(*.name).grep('Bag').elems, 1, "Map lists Bag once";
is Map.^methods.map(*.name).grep('Hash').elems, 1, "Map lists Hash once";
is Set.^methods.map(*.name).grep('SetHash').elems, 1, "Set lists SetHash once";
is Seq.^methods.map(*.name).grep('Mix').elems, 1, "Seq lists Mix once";
is Bag.^methods.map(*.name).grep('BagHash').elems, 1, "Bag lists BagHash once";
is Mix.^methods.map(*.name).grep('MixHash').elems, 1, "Mix lists MixHash once";
ok Int.^methods.map(*.name).grep('Hash').elems == 0, "Int does not list Hash";
ok Str.^methods.map(*.name).grep('Map').elems == 0, "Str does not list Map";

done-testing;
