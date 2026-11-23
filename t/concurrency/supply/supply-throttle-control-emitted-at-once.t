use Test;

# #18181: `Supply.throttle(0, &block, :$control)` waits for a `limit:N`
# directive on `$control`. The directive is usually emitted right after
# `throttle` returns, and a live Supplier with no taps keeps no history, so
# the throttle has to be listening by then. It used to start listening from a
# worker task: when the pool was busy that task ran after the emit, missed it,
# and held every job back (roast/S17-supply/throttle.t failing tests 9-10
# under load). Busy the pool first, so the old race is the common case.

plan 3;

my @busy = (^8).map: { start { my $end = now + 1; while now < $end { } } };

my $control = Supplier.new;
my $status  = Supplier.new;
my @statuses;
$status.Supply.tap: { @statuses.push: $_ };
my @seen;
my $done = Promise.new;
(1..4).Supply.throttle(0, { $_ * 10 }, :$control, :$status).tap(
    { @seen.push(.result) },
    done => { $done.keep },
);
$control.emit("limit: 2");

await Promise.anyof($done, Promise.in(20));
await @busy;
ok $done, 'the throttle finished after its directive';
# Two jobs run at once, so they finish in either order.
is @seen.sort, (10, 20, 30, 40), 'every job ran';
is @statuses.elems, 1, 'one final status';
