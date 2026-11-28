use Test;

plan 6;

# A `:=` alias of an array or hash element is the element's own container, so
# `.VAR.name` reports the OWNER (`@a`, `%k`), not the alias (#19157).

my @a = 1, 2;
my $y := @a[1];
is $y.VAR.name, '@a', 'an alias of an array element reports the array';

my %k = a => 1;
my $z := %k<a>;
is $z.VAR.name, '%k', 'an alias of a hash element reports the hash';

is @a[0].VAR.name, '@a', 'the element read directly still reports the array';

sub f($p is rw) { $p.VAR.name }
is f(@a[0]), '@a', 'an is-rw parameter bound to an element reports the array';
is f(%k<a>), '%k', 'an is-rw parameter bound to a hash element reports the hash';

$y = 7;
is @a[1], 7, 'the alias still writes through to the element';
