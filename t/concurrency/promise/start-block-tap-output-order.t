use Test;

plan 6;

# A `pass`/`ok` emitted inside a `start` / `Promise.start` block runs on a
# worker-thread clone. Its TAP line must appear in real execution order relative
# to the main thread's own test lines — not buffered and drained late, which
# would make the harness see "tests out of sequence".
#
# Structure mirrors roast/S17-promise/start.t: the worker `pass` (test 2 / 5)
# must be emitted before the main-thread `isa-ok` that follows it (test 3 / 6).

{
    my $p = Promise.start({
        pass "worker pass from Promise.start runs first";
        42
    });
    sleep 1;
    isa-ok $p, Promise, 'main-thread isa-ok after the worker pass';
    is $p.result, 42, "Promise result is correct";
}

{
    my $p = start {
        pass "worker pass from start block runs first";
        7
    };
    sleep 1;
    isa-ok $p, Promise, 'main-thread isa-ok after the start-block pass';
    is $p.result, 7, "start block result is correct";
}
