use Test;

plan 5;

# Cookie::Jar (ecosystem): `$h<missing>:delete` on a scalar-held Hash in a loop
# body deleted from the previous iteration's Hash and handed that back, so
# every element of the result was the first one.
my @seen;
for 1, 2 { my $h = %( a => $_ ); $h<b>:delete; @seen.push: $h }
is-deeply @seen.map(*<a>).List, (1, 2), 'a fresh hash per iteration survives :delete of a missing key';

my @maps = (1, 2).map: { my $h = %( a => $_ ); $h<b>:delete; $h };
is-deeply @maps.map(*<a>).List, (1, 2), 'same through map';

my @gone = (1, 2).map: { my $h = %( a => $_, b => 9 ); $h<b>:delete; $h };
is-deeply @gone.map(*.keys.List).List, (('a',), ('a',)), 'deleting an existing key';
is-deeply @gone.map(*<a>).List, (1, 2), 'and the values stay distinct';

sub f($x) { my $h = %( a => $x ); $h<b>:delete; $h }
is-deeply (f(1)<a>, f(2)<a>), (1, 2), 'in a sub called twice';
