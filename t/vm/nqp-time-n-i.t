use Test;
use nqp;

# From Test::Stream's t/lib/My/Test.rakumod, which times assertions with nqp::time_n.

plan 4;

my $n = nqp::time_n;
isa-ok $n, Num, 'nqp::time_n is a Num';
ok $n > 1_700_000_000e0, 'nqp::time_n counts seconds since the epoch';
my $i = nqp::time_i;
isa-ok $i, Int, 'nqp::time_i is an Int';
ok $i > 1_700_000_000 && $i <= nqp::time_n + 1, 'nqp::time_i counts whole seconds since the epoch';
