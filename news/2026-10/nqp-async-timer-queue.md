# `nqp::timer` and `nqp::cancel` on a blocking queue

MoarVM's asynchronous `nqp::` ops share one shape. Each takes a queue of REPR
`ConcBlockingQueue` and a handle type of REPR `AsyncTask`, and pushes its schedulee onto the
queue when the event fires. The first two ops of that family now work in mutsu (#11502,
ADR-11502):

```raku
use nqp;
my class Q is repr('ConcBlockingQueue') { }
my class TC is repr('AsyncTask') { }
my $q := nqp::create(Q);
my $h := nqp::timer($q, 'tick', 0, 20, TC);   # now, then every 20 ms
say nqp::shift($q) for ^3;                    # blocks until each tick arrives
nqp::cancel($h);
```

A queue class's instance is backed by the same channel `Channel` uses:

- `nqp::push` adds an item;
- `nqp::shift` blocks until there is one;
- `nqp::elems` counts the items waiting.

A task instance holds a cancellation flag from the registry `Cancellation` uses. The timer
is an entry on the deadline heap that `Supply.interval` and `Promise.in` already share, so
it adds no thread. On the timer thread it only pushes the schedulee, and stops once
`nqp::cancel` sets the flag. `.REPR` reports both REPRs for the class and for its
instances, and the errors match Rakudo's.

The rest of the family (`signal`, the process ops, the socket ops, `permit` and
`watchfile`) needs a common sink over mutsu's native implementations first. ADR-11502
records that plan.
