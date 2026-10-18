use Test;

# A lazy receiver's methods run through the lazy step (ADR-13891): a method's
# pull answers from what it reads of the iterator (`AT-POS` up to its index,
# `cache` keeps the iterator, the placeholders of `gist`/`Str`/`raku`/`join`),
# and a method that needs every element reifies the list first. The calls go
# through a dynamic method name too, which reaches the by-name dispatcher.

plan 24;

my @a = 1..*;
is @a.gist, '[...]', 'gist of a lazy array is its placeholder';
is @a.raku, '[...]', 'raku of a lazy array is its placeholder';
is @a.Str, '...', 'Str of a lazy array is ...';
is @a.join(','), '...', 'join of a lazy array is ...';
is @a.AT-POS(2), 3, 'AT-POS of a lazy array pulls up to its index';

is (1..*).list.gist, '(...)', 'gist of a lazy list is (...)';
is (1...*).gist, '(...)', 'gist of an infinite Seq';
is (1...*).Str, '...', 'Str of an infinite Seq';
is (1...*).join, '...', 'join of an infinite Seq';
is (1...*).map(* + 1).join(','), '...', 'join of a lazy map';
is (1...*).AT-POS(3), 4, 'AT-POS of an infinite Seq';
ok (1...*).EXISTS-POS(3), 'EXISTS-POS of an infinite Seq';
ok (1..*).map(* * 2).EXISTS-POS(10), 'EXISTS-POS of a lazy map';
is (1, 2 ... 10).lazy.AT-POS(2), 3, 'AT-POS of a finite lazy Seq';

isa-ok (1...*).cache, List, 'cache of an infinite Seq is a List';
my $c = (1...*).cache;
is $c[5], 6, 'the cached view pulls on demand';

my $g = gather { take 1; take 2 };
is $g.join('-'), '1-2', 'join of a gather that ends reifies it';
is (gather { take 1; take 2 }).gist, '(1 2)', 'gist of a gather that ends';
is (gather { take 1; take 2 }).elems, 2, 'elems reifies the gather';

my $m = 'AT-POS';
is (1...*)."$m"(4), 5, 'AT-POS through a dynamic name';
$m = 'gist';
is (1...*)."$m"(), '(...)', 'gist through a dynamic name';
$m = 'join';
is (1...*)."$m"(','), '...', 'join through a dynamic name';
$m = 'cache';
isa-ok (1...*)."$m"(), List, 'cache through a dynamic name';
$m = 'elems';
is (gather { take $_ for 1..3 })."$m"(), 3, 'elems of a gather through a dynamic name';
