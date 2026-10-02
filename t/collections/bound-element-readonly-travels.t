use Test;

# A literal `:=` bind stores a read-only element cell, so the restriction
# travels with the container whatever name reaches it (#11021).

plan 8;

my @a = 1, 2, 3; @a[1] := 42;
my @b := @a;
dies-ok { @b[1] = 9 }, 'alias sees the read-only element';
sub f(@x) { @x[1] = 9 }
dies-ok { f(@a) }, 'parameter sees the read-only element';
is @a.join(','), '1,42,3', 'value preserved';

my %h; %h<k> := 5;
sub g(%x) { %x<k> = 9 }
dies-ok { g(%h) }, 'hash parameter sees the read-only entry';
is %h<k>, 5, 'hash value preserved';

my @d = 1, 2, 3; @d[0] := 7; @d = 4, 5, 6; @d[0] = 8;
is @d.join(','), '8,5,6', 'whole reassignment makes elements writable';

my @e = 1, 2, 3; @e[0] := 7; @e[0]:delete; @e[0] = 8;
is @e.join(','), '8,2,3', 'delete frees the slot';

my $v = 3; my @f; @f[0] := $v; @f[0] = 5;
is $v, 5, 'bind to a variable stays writable-through';
