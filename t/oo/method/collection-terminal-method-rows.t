use Test;

# The list terminals are rows on the owners Rakudo declares them on (ADR-11276,
# the collection terminals): `List.head` / `List.tail` / `Array.tail` /
# `Map.head` (counted forms), `List.sum`, and `flat` on `Map` and `Range`. Each row
# shares the handler `Any` / `List` already used, so a receiver it declines (a lazy
# list, a hash that reads as pairs) answers as before.

plan 18;

my @a = 1..6;
my $l = (1, 2, 3, 4, 5);

is-deeply @a.head(2).list, (1, 2), 'Array.head(n)';
is-deeply $l.head(2).list, (1, 2), 'List.head(n)';
is-deeply @a.tail(2).list, (5, 6), 'Array.tail(n)';
is-deeply $l.tail(2).list, (4, 5), 'List.tail(n)';
is-deeply @a.head(100).list, (1, 2, 3, 4, 5, 6), 'head past the end';
is-deeply @a.tail(0).list, (), 'tail(0)';

my %h = a => 1;
is-deeply %h.head(1).list, (a => 1,), 'Hash.head(n) reads the pairs';
is-deeply Map.new((a => 1)).head(1).list, (a => 1,), 'Map.head(n)';

is $l.sum, 15, 'List.sum';
is @a.sum, 21, 'Array.sum';
is (1.5, 2.5).sum, 4, 'List.sum of Rats';
is (1..4).sum, 10, 'Range.sum';

is-deeply Map.new((a => 1)).flat.list, (a => 1,), 'Map.flat';
is-deeply (1..3).flat.list, (1, 2, 3), 'Range.flat';
is-deeply ((1, 2), (3, 4)).flat.list, (1, 2, 3, 4), 'List.flat';

is (1..Inf).head(2).list.elems, 2, 'a lazy range still takes the first two';

ok List.^can('head') && List.^can('tail') && List.^can('sum'), '.^can sees the List rows';
ok Map.^can('head') && Map.^can('flat') && Range.^can('flat'), '.^can sees the Map and Range rows';
