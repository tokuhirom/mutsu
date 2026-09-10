use Test;

# Supply.batch(:seconds) must bucket by absolute wall-clock period
# (rakudo's `time div $seconds`), not by elapsed time since tap
# registration / last flush. The registration-anchored variant fired a
# spurious 1-element time-flush on the second emit whenever the tap was
# registered just after a period boundary — the intermittent
# S17-supply/batch.t "we can batch by time and elems" failure. This test
# forces that boundary alignment, making the old bug deterministic.

plan 2;

my $seconds = 2;
my $elems   = 7;
my $spurt   = 10;

sleep $seconds - now % $seconds;   # land just after a period boundary
my $s = Supplier.new;
my $b = $s.Supply.batch( :$elems, :$seconds );   # register at boundary+eps
sleep $seconds - now % $seconds;   # ~full period until the next boundary
my $base = time div $seconds;

my @res;
my $done;
$b.tap({ @res.push($_) }, :done({ $done = True }));
$s.emit( time div $seconds ) for ^$spurt;
sleep $seconds;
$s.emit( time div $seconds ) for ^$spurt;
$s.done;
for ^100 { last if $done; sleep .1 }

is @res.map(*.elems).join(","), "7,3,7,3",
    'batch(:elems, :seconds) splits on elems and period boundaries only';
is-deeply @res,
    [($base xx $elems).List, ($base xx $spurt - $elems).List,
     ($base+1 xx $elems).List, ($base+1 xx $spurt - $elems).List],
    'batched values match their emission periods';
