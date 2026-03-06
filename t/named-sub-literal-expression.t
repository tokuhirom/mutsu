use Test;

plan 2;

my $x = (sub f { 41 + 1 }).();
is $x, 42, 'named sub literal can be called with .()';

my $y = (sub add ($a, $b) { $a + $b }).(20, 22);
is $y, 42, 'named sub literal with params can be called with .()';
