use Test;

# #19322: an rw `for` over a shaped array reached through an expression
# aliases its elements, as one over the named variable does.
plan 6;

my @s[3] = 1, 2, 3;
my $q = @s;
for @$q <-> $x { $x += 1 }
is-deeply @s.List, (2, 3, 4), '@$q over a shaped array scalar writes back';

for $q.list -> $x is rw { $x *= 10 }
is-deeply @s.List, (20, 30, 40), '$q.list with is rw writes back';

my @a[2;3] = (1, 2, 3), (4, 5, 6);
for @a[1] <-> $x { $x *= 10 }
is-deeply @a[0].List, (1, 2, 3), 'the other row is untouched';
is-deeply @a[1].List, (40, 50, 60), 'a row view of a multi-dimensional array writes back';

my $row = @a[0];
for @$row <-> $x { $x += 100 }
is-deeply @a[0].List, (101, 102, 103), '@$row writes back into the parent';

my $r = [1, 2];
for @$r <-> $x { $x *= 10 }
is-deeply $r.List, (10, 20), 'a plain Array through a scalar still writes back';
