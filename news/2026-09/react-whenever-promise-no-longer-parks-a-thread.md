# `whenever <Promise>` no longer parks an OS thread per subscription

Issue [#7609](https://github.com/tokuhirom/mutsu/issues/7609) tracks a rare, never-reproduced
SIGSEGV in `roast/S17-procasync/stress.t`, whose last block runs the
[rakudo#3299](https://github.com/rakudo/rakudo/issues/3299) regression: 1200 `Proc::Async`
instances, each started inside its own `react`.

```raku
for ^1200 {
  my $proc = Proc::Async.new('cat', '/dev/null');
  react {
      whenever $proc.start { done }
      whenever signal(SIGTERM) {}
      whenever Promise.in(5) {}
  }
}
```

The issue's own "where to look next" list put *"the `react` / `whenever` machinery itself under
1200 rapid setup/teardown cycles"* first. Measuring the workload rather than reading it found
something concrete there.

## What the measurement showed

Sampling `/proc/<pid>/task/*/comm` at the peak of a debug run:

```
peak=425
    420 promise-wait
      1 timer
      1 signal-rd
      1 pool
      1 mutsu-main
      1 mutsu
```

Every `whenever <Promise>` source built by the react loop (`vm/vm_react_loop.rs`) and by the
`supply { whenever $promise { … } }` path (`runtime/supply_promise.rs`) spawned a dedicated
`promise-wait` OS thread whose entire job was to block in `SharedPromise::wait()` and then push
`Emit`/`Done` (or `Quit`) into a one-shot channel.

For `whenever Promise.in(5)` that thread stays parked for the timer's **whole five seconds** — long
after its react block ended, milliseconds later, via `done`. At ~70 iterations a second the workload
therefore carried roughly 420 abandoned threads at once, each holding its default stack, which is
where the 4.5 GB of address space came from.

## The fix

A `SharedPromise` already has a waiter list, and the drive loop already uses it: every promise
subscription registers `p.on_resolve(|…| waker.notify())` so the loop wakes on resolution
(`vm/vm_react_subscriptions.rs`). The value delivery now rides the same list instead of a thread:

```rust
shared.mark_observed();
shared.on_resolve(Box::new(move |status, result, _output, _stderr| {
    if status == "Broken" {
        let _ = tx.send(SupplyEvent::Quit(result));
    } else {
        let _ = tx.send(SupplyEvent::Emit(result));
        let _ = tx.send(SupplyEvent::Done);
    }
}));
```

`mark_observed` is what `wait()` used to do implicitly: a Broken promise consumed by a `whenever`
must not also trip the destruction-time "unhandled" diagnostic. The waiter itself runs either inline
on the registering thread (promise already resolved) or on a pooled worker via
`SharedPromise::dispatch_waiters` — both registered GC mutators, which the `Value` clone inside
`send` requires.

Delivery ordering improves as a side effect: the value-sending waiter is registered before the
drive loop's waker-notifying one, so the event is in the channel before the loop is woken, instead
of racing it.

## Measurements

Debug build, `tmp/segv-3299.raku` (the rakudo#3299 block standalone), 4-core container:

| | before | after |
| --- | --- | --- |
| peak OS threads | 425 | 6 |
| peak `VmSize` | 4.6 GB | 0.9 GB |
| wall clock | 17.1 s | 3.8 s |
| system time | 42.2 s | 1.5 s |

## What this does and does not settle

It does **not** root-cause #7609's SIGSEGV — that crash has still never been reproduced, and the
issue stays open. What it removes is the largest piece of resource pressure the crashing workload
was under, and one of the three candidate mechanisms the issue named. A file that ran with 420
concurrent threads and 4.5 GB of address space now runs with six threads; whatever remains to be
explained is being asked of a much smaller system.

Pinned by `t/concurrency/supply/react-whenever-promise-threads.t`, which fails (test 1, thread
count) against the pre-fix binary.
