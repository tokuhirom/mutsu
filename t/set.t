use Test;
plan 60;

# --- Set ---

# Construction
my $s = set(<a b c>);
isa-ok $s, Set, "set() returns a Set";
is $s.elems, 3, "set has 3 elements";

# Set.new
my $s2 = Set.new("x", "y", "z");
isa-ok $s2, Set, "Set.new returns a Set";
is $s2.elems, 3, "Set.new has 3 elements";

# Duplicates removed
my $s3 = set(<a b a c b>);
is $s3.elems, 3, "duplicates removed in set";

# Subscript access (returns Bool)
ok $s<a>, "set subscript for existing element is True";
nok $s<z>, "set subscript for missing element is False";

# .keys
my @k = $s.keys.sort;
is @k.elems, 3, ".keys has 3 elements";

# .values (all True for Set)
my @v = $s.values;
is @v.elems, 3, ".values has 3 elements";
ok @v[0], "set values are truthy";

# .kv
my @kv = $s.kv;
is @kv.elems, 6, ".kv has 6 elements (3 pairs)";

# .pairs
my @p = $s.pairs;
is @p.elems, 3, ".pairs has 3 elements";

# .WHAT
is $s.WHAT, "(Set)", ".WHAT returns (Set)";

# .gist
ok $s.gist.contains("set"), ".gist contains 'set'";

# .Bool / truthiness
ok ?$s, "non-empty set is truthy";
nok ?set(), "empty set is falsy";

# Equality
ok set(<a b c>) == set(<c b a>), "sets with same elements are equal";
nok set(<a b>) == set(<a b c>), "sets with different elements are not equal";

# --- Bag ---

# Construction
my $b = bag(<a b a c a b>);
isa-ok $b, Bag, "bag() returns a Bag";
is $b.elems, 3, "bag has 3 distinct elements";
is $b.total, 6, "bag total is 6";

# Bag.new
my $b2 = Bag.new("x", "y", "x");
isa-ok $b2, Bag, "Bag.new returns a Bag";
is $b2.elems, 2, "Bag.new has 2 distinct elements";
is $b2.total, 3, "Bag.new total is 3";

# Subscript access (returns count)
is $b<a>, 3, "bag subscript returns count for 'a'";
is $b<b>, 2, "bag subscript returns count for 'b'";
is $b<c>, 1, "bag subscript returns count for 'c'";
is $b<z>, 0, "bag subscript returns 0 for missing";

# .keys
my @bk = $b.keys.sort;
is @bk.elems, 3, "bag .keys has 3 elements";

# .values
my @bv = $b.values.sort;
is @bv.elems, 3, "bag .values has 3 elements";

# .kv
my @bkv = $b.kv;
is @bkv.elems, 6, "bag .kv has 6 elements";

# .WHAT
is $b.WHAT, "(Bag)", ".WHAT returns (Bag)";

# Truthiness
ok ?$b, "non-empty bag is truthy";
nok ?bag(), "empty bag is falsy";

# --- Mix ---

# Construction
my $m = mix(<a b a c a b>);
isa-ok $m, Mix, "mix() returns a Mix";
is $m.elems, 3, "mix has 3 distinct elements";
is $m.total, 6, "mix total is 6";

# Mix.new
my $m2 = Mix.new("x", "y", "x");
isa-ok $m2, Mix, "Mix.new returns a Mix";
is $m2.elems, 2, "Mix.new has 2 distinct elements";

# Subscript access (returns Num weight)
is $m<a>, 3, "mix subscript returns weight for 'a'";
is $m<z>, 0, "mix subscript returns 0 for missing";

# .WHAT
is $m.WHAT, "(Mix)", ".WHAT returns (Mix)";

# Truthiness
ok ?$m, "non-empty mix is truthy";
nok ?mix(), "empty mix is falsy";

# --- Coercions ---

# Array to Set
my @arr = <x y z x>;
my $from-arr = @arr.Set;
isa-ok $from-arr, Set, "Array.Set returns a Set";
is $from-arr.elems, 3, "Array.Set removes duplicates";

# Array to Bag
my $from-arr-bag = @arr.Bag;
isa-ok $from-arr-bag, Bag, "Array.Bag returns a Bag";
is $from-arr-bag<x>, 2, "Array.Bag counts duplicates";

# Set to Bag
my $set-to-bag = set(<a b c>).Bag;
isa-ok $set-to-bag, Bag, "Set.Bag returns a Bag";
is $set-to-bag<a>, 1, "Set.Bag gives weight 1";

# Bag to Set
my $bag-to-set = bag(<a b a>).Set;
isa-ok $bag-to-set, Set, "Bag.Set returns a Set";
is $bag-to-set.elems, 2, "Bag.Set has correct elems";

# Bag to Mix
my $bag-to-mix = bag(<a b a>).Mix;
isa-ok $bag-to-mix, Mix, "Bag.Mix returns a Mix";
is $bag-to-mix<a>, 2, "Bag.Mix preserves weight";

# Mix to Set
my $mix-to-set = mix(<a b a>).Set;
isa-ok $mix-to-set, Set, "Mix.Set returns a Set";
is $mix-to-set.elems, 2, "Mix.Set has correct elems";

# Mix .kv
my @mkv = $m.kv;
is @mkv.elems, 6, "mix .kv has 6 elements";

# Mix .pairs
my @mp = $m.pairs;
is @mp.elems, 3, "mix .pairs has 3 elements";

# Mix to Bag
my $mix-to-bag = mix(<a b a>).Bag;
isa-ok $mix-to-bag, Bag, "Mix.Bag returns a Bag";

# Set .pairs values are Pairs
my @sp = set(<x y>).pairs;
is @sp.elems, 2, "set .pairs returns correct count";
