use Test;

# `done-testing` emits `1..N` for the number of tests that actually ran. Once a
# test has run on a spawned thread (a `start` block, a Promise or Supply
# callback) the count lives in a shared atomic, and the main thread's own
# counter is stale — so the emitted plan must come from the shared one.

ok True, 'main thread test';

my $s = Supplier.new;
my $done = Promise.new;
my $n = 0;
$s.Supply.tap: -> $v {
    ok $v > 0, "tap test $v";
    $done.keep if ++$n == 3;
};
start { $s.emit($_) for 1..3; $s.done }
await Promise.anyof($done, Promise.in(10));

# Runs 4 tests in total; prove fails the file unless the trailing plan says 1..4.
done-testing;
