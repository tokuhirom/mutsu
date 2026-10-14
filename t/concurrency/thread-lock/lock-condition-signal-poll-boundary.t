use Test;

# #13184: the main program's thread waits on a Condition in 10ms slices
# (so it can adopt a worker's `exit`). A signal landing just as a slice timed
# out was reported by std's Condvar as a timeout, and the waiter went back to
# sleep for a signal that had already been delivered: a rare hang. The signal
# delay is swept across several slices so signals land on both sides of a
# slice boundary. The wait runs on the main thread (the one that polls); a
# watchdog `exit`s the file if a signal is ever lost, so it fails instead of
# hanging.

plan 1;

my $watchdog = Thread.start({ sleep 90; note 'watchdog: a Condition signal was lost'; exit 1 });

for ^120 -> $i {
    my $lock = Lock.new;
    my $condition = $lock.condition;
    my $done = 0;
    my $delay = 0.0005 * ($i % 40) + 0.0025;
    Thread.start({
        sleep $delay;
        $lock.protect({
            $done = 1;
            $condition.signal;
        });
    });
    $lock.protect({ $condition.wait({ $done == 1 }) });
}
pass 'no signal is lost across 120 waits that straddle poll slices';
exit 0;
