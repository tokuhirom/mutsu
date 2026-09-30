use Test;
plan 4;

my $supplier = Supplier.new;
my $quit-count = 0;
$supplier.Supply.tap({ die 'tap failed' }, quit => { $quit-count++ });

dies-ok { $supplier.emit(1) }, 'a tap callback failure reaches the emitter';
is $quit-count, 0, 'the tap quit handler does not handle its own callback failure';

$supplier.quit('source failed');
is $quit-count, 1, 'an explicit source quit still reaches the tap quit handler';

my $without-quit = Supplier.new;
$without-quit.Supply.tap({ die 'unhandled tap failure' });
dies-ok { $without-quit.emit(1) }, 'a callback failure propagates without a quit handler';
