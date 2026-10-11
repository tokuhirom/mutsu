use Test;
plan 21;

# `Str` of the quant hashes and `clone` of the mutable ones are method-table
# rows (ADR-11276 §9.73). Expectations checked against raku; the keys are
# sorted before comparing where the order is a hash order.
sub words($s) { $s.words.sort.join(" ") }

is words(set(<a b>).Str), "a b", 'Set.Str joins the elements';
is words(SetHash.new(<a b>).Str), "a b", 'SetHash.Str';
is words(bag(<a a b>).Str), "a(2) b", 'Bag.Str shows a weight above 1';
is words(BagHash.new(<a a b>).Str), "a(2) b", 'BagHash.Str';
is words((a => 1.5, b => -2).Mix.Str), "a(1.5) b(-2)", 'Mix.Str shows each weight';
is words(MixHash.new(<a b>).Str), "a b", 'MixHash.Str';
is set().Str, "", 'an empty Set is the empty string';
is ~bag(<x>), "x", 'prefix ~ agrees';

# A Bag or Mix renders an element by its gist, a Set by its Str.
is bag("a" => 1).Str, "a => 1", 'Bag.Str gists a Pair element';
is set("a" => 1).Str, "a\t1", 'Set.Str stringifies a Pair element';
is bag($[1, 2]).Str, "[1 2]", 'Bag.Str gists an itemized Array element';
is set($[1, 2]).Str, "1 2", 'Set.Str stringifies it';
is ((a => 1) => 2).Mix.Str, "a => 1(2)", 'Mix.Str gists a Pair element';

my $s = SetHash.new(<a b>);
my $c = $s.clone;
$c<z> = True;
is $s.keys.sort.join(","), "a,b", 'SetHash.clone: the original is untouched';
is $c.keys.sort.join(","), "a,b,z", 'SetHash.clone: the copy changed';
isa-ok $c, SetHash, 'SetHash.clone is a SetHash';

my $b = BagHash.new(<a a>);
my $d = $b.clone;
$d<a>++;
is ($b<a>, $d<a>).join(","), "2,3", 'BagHash.clone is independent';
nok $d === $b, 'BagHash.clone is a new object';

my $m = MixHash.new(<a>);
my $e = $m.clone;
$e<a> = 0.5;
is ($m<a>, $e<a>).join(","), "1,0.5", 'MixHash.clone is independent';

is SetHash[Int].new(1).clone.^name, "SetHash[Int]", 'the type parameter survives';
my %h is SetHash = <a b>;
isa-ok %h.clone, SetHash, 'clone of an "is SetHash" variable';
