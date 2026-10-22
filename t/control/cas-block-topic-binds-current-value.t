use Test;
plan 4;

my $t = 'p';
cas $t, { $_ ~ 'a' };
cas $t, { $_ ~ 'b' };
cas $t, { $_ ~ 'c' };
is $t, 'pabc', 'each cas block sees the value the previous one stored as $_';

my $n = 1;
cas $n, -> $v { $v * 3 };
is $n, 3, 'a pointy block gets the current value as its parameter';

my $s = 'x';
cas $s, { .uc };
is $s, 'X', 'method call on the implicit topic';

sub f { try { die "boom" }; my $v = 'p'; cas $v, { $_ ~ 'q' }; $v }
is f(), 'pq', 'the topic is the cas target, not an outer $_';
