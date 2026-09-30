use Test;

plan 5;

# In a signature declaration (`my ($x, $y) = RHS`) the new variables are
# already in scope on the RHS and hold `Any` (#10173, follow-up to #9770).

my $x = 5;
{ my ($x, $y) = $x, 2; ok !$x.defined, 'RHS reads the new $x, not the outer one'; is $y, 2, 'other target assigned' }
{ my ($x, $y) = (do { $x }), 2; ok !$x.defined, 'nested block on the RHS sees the new $x' }

my ($a, $b) = $a, 2;
ok !$a.defined, 'no outer symbol: reads Any';
{ my ($p, @q) = 1, 2, 3; is @q.join(","), "2,3", 'array target still slurps' }

done-testing;
