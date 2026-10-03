use Test;

# An `ENTER` expression inside a `LEAVE` phaser runs on entry to the enclosing
# block, so `LEAVE take now - ENTER now` measures the block (Benchmark's
# `timethis(..., :statistics)`): the duration is positive.

plan 4;

my @t = gather for ^2 { LEAVE take now - ENTER now; sleep 0.02 };
ok @t.elems == 2 && @t.all > 0, 'loop body: LEAVE take now - ENTER now';

my $d;
sub f { LEAVE $d = now - ENTER now; sleep 0.02 }
f();
ok $d > 0, 'routine body';

my $e;
for ^1 { LEAVE { $e = now - ENTER now }; sleep 0.02 }
ok $e > 0, 'LEAVE with a block body';

my $order = '';
for ^1 { LEAVE { $order ~= ENTER { 'E' } }; $order ~= 'b' }
is $order, 'bE', 'the ENTER value is computed at entry and read at exit';
