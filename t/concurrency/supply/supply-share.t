use Test;

plan 6;

# MoarVM::Remote uses Supply.share to let its unpacker and diagnostics tap the
# same live byte stream without running the on-demand supply twice.
my $source = Supplier.new;
my $runs = 0;
my $cold = supply {
    $runs++;
    whenever $source.Supply {
        emit $_;
    }
};
my $shared = $cold.share;
my (@left, @right);
my ($left-done, $right-done) = False xx 2;

$shared.tap({ @left.push($_) }, done => { $left-done = True });
$shared.tap({ @right.push($_) }, done => { $right-done = True });
$source.emit(10);
$source.emit(20);
$source.done;

is $runs, 1, 'the shared on-demand source runs once';
is-deeply @left, [10, 20], 'the first tap receives every shared value';
is-deeply @right, [10, 20], 'the second tap receives every shared value';
ok $left-done && $right-done, 'both shared taps receive completion';

my @bytes;
sub consume-bytes(Buf $chunk) {
    $chunk.map({ @bytes.push($_) });
}
my $byte-supplier = Supplier.new;
$byte-supplier.Supply.tap(-> $chunk { consume-bytes($chunk) });
$byte-supplier.emit(Buf.new(65, 66));
sleep 0.1;
is-deeply @bytes, [65, 66], 'discarded tap callback results sink deferred Buf.map';

my @parts = Blob.new(0xa1, 0x61).list, Blob.new(1).list;
is-deeply Blob.new(0x81, @parts.map(*.list)).list.Array, [0x81, 0xa1, 0x61, 1],
    'Blob.new flattens nested mapped byte lists';
