use Test;

# ADR-11276: WHICH on the collections, gist/raku on the quant hashes and
# raku on Range are method-table rows.

plan 22;

is Set.new(<a b>).gist, "Set(a b)", "Set.gist";
is Bag.new(<a a b>).gist, "Bag(a(2) b)", "Bag.gist";
is (a => 0.5).Mix.gist, "Mix(a(0.5))", "Mix.gist";
is Set.new("a").raku, 'Set.new("a")', "Set.raku";
is (a => 2).Bag.raku, '("a"=>2).Bag', "Bag.raku";
is <a b>.SetHash.gist, "SetHash(a b)", "SetHash.gist";
is <a>.BagHash.raku, '("a"=>1).BagHash', "BagHash.raku";

is (1..5).raku, "1..5", "Range.raku";
is (^5).raku, "^5", "short Range.raku";
is ("a".."c").raku, '"a".."c"', "string Range.raku";
is (1..5).gist, "1..5", "Range.gist";

my @a = 1, 2;
my @b = 1, 2;
isa-ok @a.WHICH, ObjAt, "Array.WHICH is an ObjAt";
ok @a.WHICH === @a.WHICH, "one Array keeps its identity";
ok @a.WHICH !=== @b.WHICH, "two Arrays differ";
my %h = a => 1;
ok %h.WHICH ~~ ObjAt, "Hash.WHICH is an ObjAt";
isa-ok (a => 1).WHICH, ValueObjAt, "Pair of values is a ValueObjAt";
ok (a => 1).WHICH eq (a => 1).WHICH, "equal Pairs share an identity";
ok Set.new(<a b>).WHICH eq Set.new(<b a>).WHICH, "equal Sets share an identity";
ok Set.new(<a b>).WHICH ne Set.new(<a>).WHICH, "different Sets differ";
ok Bag.new(<a a>).WHICH ne Bag.new(<a>).WHICH, "Bag counts matter";
is (1..2).WHICH.Str, "Range|1..2", "Range.WHICH";
is Set.WHICH.Str.starts-with("Set|U"), True, "Set type object WHICH";
