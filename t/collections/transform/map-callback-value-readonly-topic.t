use Test;
# Iter::Able t/29-for-each: a callback that arrives as a value still gets a
# read-only `$_` when the receiver is an immutable List.
plan 6;
my &f = { --$_ };
my $t = (2, 3);
ok !(try { $t.map(&f).sink; True }), '$list.map(&f) dies on --$_';
sub fe(\it, &function --> Nil) { it.map(&function).sink }
ok !(try { fe((2, 3), { --$_ }); True }), 'raw-param List receiver dies';
ok !(try { $t.map({ --$_ }).sink; True }), 'literal block still dies';
my @a = 1, 2;
my &g = { $_ *= 2 };
@a.map(&g).sink;
is @a, [2, 4], '@a.map(&g) writes through';
@a.list.map(&g).sink;
is @a, [4, 8], '@a.list.map(&g) writes through';
is (1, 2).map(&{ $_ + 1 }).join(","), "2,3", 'read-only use is fine';
