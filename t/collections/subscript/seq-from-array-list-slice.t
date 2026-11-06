use Test;

plan 4;

# A Seq made from an Array keeps the Array's element cells; a list slice of it
# still answers the elements at those positions.
is-deeply [5, 6, 7].Seq[0, 2], (5, 7), 'list slice of an Array-derived Seq';
my @a = 5, 6, 7;
is-deeply @a.Seq[2, 1, 9], (7, 6, Nil), 'out-of-range positions are Nil';
my $s := [0, -1, -2, -3].Seq;
is $s.elems, 4, '.elems does not consume it';
is-deeply $s.[3, 1], (-3, -1), 'and a slice after .elems still reads';
