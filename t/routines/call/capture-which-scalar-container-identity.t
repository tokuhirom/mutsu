use Test;
plan 5;

my class P { }
sub which-of(|c) { c.WHICH }

my $m = P;
is which-of(:base($m)), which-of(:base(P)), 'a scalar holding a type object has the type\'s identity in a named Capture part';
is which-of(:base($m)), which-of(:base($m)), 'and the identity is stable across calls';
like which-of(:base($m)), /^ 'Capture|base(P|U' \d+ ')' $/, 'rendered as the type object';

my $n = 42;
is which-of(:k($n)), which-of(:k(42)), 'a named scalar holding an Int';

my $s = "str";
is $s.WHICH, "str".WHICH, 'WHICH of a scalar variable is its content\'s';
