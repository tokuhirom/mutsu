use Test;
plan 10;

my @a = 3, 1, 2;
is @a.elems, 3, ".elems";
is @a.head, 3, ".head";
is @a.tail, 2, ".tail";
is @a.end, 2, ".end";
is @a.min, 1, ".min";
is @a.max, 3, ".max";
is @a.reverse.head, 2, ".reverse";
is @a.sort.head, 1, ".sort";

my @b = 1, 2, 2, 3;
is @b.unique.elems, 3, ".unique";

my @c = 1, 2, 3;
my @d = @c.map(-> $x { $x * 2 });
is @d.elems, 3, ".map";
