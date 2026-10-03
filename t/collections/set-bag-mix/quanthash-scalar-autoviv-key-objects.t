use Test;

plan 7;

# A typed QuantHash scalar still holding its type object autovivifies on
# the first store and keeps that key's element object.
my MixHash $m;
my @pop = $(True, False), $(False, True);
for @pop -> $p { $m{$p} = 1 }
is $m.keys.map(*.^name).sort, <List List>, 'MixHash first-store key stays a List';
is $m.Mix.keys.map(*.^name).sort, <List List>, '.Mix keeps the List keys';

my BagHash $b;
$b{$(1, 2)} = 3;
is $b.keys.map(*.^name), <List>, 'BagHash first-store key stays a List';

my SetHash $s;
$s{2} = True;
is $s.keys.map(*.^name), <Int>, 'SetHash first-store key stays an Int';

# A slice store into the type object autovivifies too.
my MixHash $ms;
$ms{(True, False)} = 2, 0;
is $ms.keys.map(*.^name), <Bool>, 'MixHash slice store keeps Bool keys';
my BagHash $bs;
$bs{(1, 2)} = 3, 4;
is $bs.keys.map(*.^name).sort, <Int Int>, 'BagHash slice store keeps Int keys';
is $bs{2}, 4, 'BagHash slice store distributes the values';
