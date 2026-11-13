use Test;

# A Supplier that emits many values delivers each one to its taps at a
# cost that does not grow with the values already emitted: an emit used to
# copy the supplier's whole emitted history (twice), which made a byte-at-a-
# time stream decoder (Data::MessagePack's StreamingUnpacker) quadratic.

plan 6;

my $supplier = Supplier.new;
my $sum = 0;
my $count = 0;
$supplier.Supply.tap(-> $v { $sum += $v; $count++ });
$supplier.emit($_) for ^30000;
is $count, 30000, 'every emitted value reaches the tap';
is $sum, [+](^30000), 'in full';

my $late = 0;
$supplier.Supply.tap(-> $ { $late++ });
$supplier.emit(1);
is $late, 1, 'a later tap sees only later values';

my $p = Supplier::Preserving.new;
$p.emit($_) for ^5;
my @got;
$p.Supply.tap(-> $v { @got.push: $v });
$p.emit(5);
is @got, [0, 1, 2, 3, 4, 5], 'a preserving supplier replays its backlog to the first tap';

my $done = False;
my $s2 = Supplier.new;
$s2.Supply.tap(-> $ { }, done => { $done = True });
$s2.emit(1);
$s2.done;
ok $done, 'done still reaches the tap';
$s2.emit(2);
pass 'an emit after done is ignored';
