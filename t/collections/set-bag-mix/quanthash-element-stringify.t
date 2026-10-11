use Test;

plan 9;

# A Set/Bag/Mix renders each object element through the element's own
# method: Setty.Str uses Str, Baggy/Mixy.Str and every .gist use gist.
class F { method gist { "G" }; method Str { "S" } }

is set(F.new).Str, "S", 'Set.Str uses the element Str';
is ~set(F.new), "S", 'prefix ~ on a Set uses the element Str';
my $s = set(F.new);
is "$s", "S", 'interpolating a Set uses the element Str';
is (F.new => 2).Bag.Str, "G(2)", 'Bag.Str uses the element gist';
is (F.new => 2.5).Mix.Str, "G(2.5)", 'Mix.Str uses the element gist';
is set(F.new).gist, "Set(G)", 'Set.gist uses the element gist';
is (F.new => 2).Bag.gist, "Bag(G(2))", 'Bag.gist uses the element gist';
is-deeply set(<a b>).Str, "a b", 'plain elements are unchanged';
is (a => 2).Bag.gist, "Bag(a(2))", 'plain Bag gist is unchanged';
