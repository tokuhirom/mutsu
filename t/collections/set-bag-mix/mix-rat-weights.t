use Test;

plan 25;

# A Mix weight keeps its own Real type (#12703).
my $m = (a => 1/3).Mix;
is $m<a>.^name, 'Rat', 'a Rat weight stays a Rat';
is $m.Str, 'a(0.333333)', '.Str shows the Rat weight';
is $m.gist, 'Mix(a(0.333333))', '.gist shows the Rat weight';
is $m.raku, '("a"=><1/3>).Mix', '.raku keeps the exact Rat';
is (a => 0.5e0).Mix<a>.^name, 'Num', 'a Num weight stays a Num';
is (a => 2).Mix<a>.^name, 'Int', 'an Int weight stays an Int';
is (a => 3.14).Mix<a>.^name, 'Rat', 'a decimal literal weight is a Rat';

is (a => 1/3, a => 1/3).Mix<a>, 2/3, 'duplicate keys sum exactly';
is (a => 1/3, a => 1/3).Mix<a>.^name, 'Rat', '... and stay a Rat';

my $mh = MixHash.new;
$mh<a> = 1/3;
is $mh<a>.^name, 'Rat', 'assigning a Rat keeps it';
$mh<a> += 1/3;
is $mh<a>, 2/3, '+= adds exactly';
$mh<b>++;
is $mh<b>.^name, 'Int', '++ on a fresh key is an Int';

is ($m (+) (b => 1/6).Mix)<a>, 1/3, '(+) keeps the Rat weight';
is ($m (+) (b => 1/6).Mix)<b>.^name, 'Rat', '(+) result weight is a Rat';
is $m.total, 1/3, '.total of a Rat weight is exact';
is $m.pairs[0].value.^name, 'Rat', '.pairs yields the Rat';
is $m.kv[1].^name, 'Rat', '.kv yields the Rat';

# Weights with no 64-bit form keep their type (#12737).
my $big = (a => 2**70).Mix;
is $big<a>.^name, 'Int', 'a big Int weight stays an Int';
is $big<a>, 2**70, '... and keeps its value';
my $fat = (a => FatRat.new(1, 3)).Mix;
is $fat<a>.^name, 'FatRat', 'a FatRat weight stays a FatRat';
is $fat<a>, FatRat.new(1, 3), '... and keeps its value';
is ($big (+) (a => 1).Mix)<a>, 2**70 + 1, '(+) adds a big weight exactly';
is (a => 2**70, a => 2**70).Mix<a>, 2**71, 'duplicate keys sum a big weight exactly';
my $mh = MixHash.new;
$mh<a> = 2**70;
is $mh<a>, 2**70, 'assigning a big Int stores it';
is $mh.total, 2**70, '.total of a big weight is exact';
