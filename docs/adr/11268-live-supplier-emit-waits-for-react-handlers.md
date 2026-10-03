# ADR-11268: A live Supplier's emit waits for the reacts that tap it

- **Status**: Proposed (2026-10-03). Implemented in the PR that closes #11268; awaiting the
  maintainer's acceptance of the delivery model.
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11268](https://github.com/tokuhirom/mutsu/issues/11268)
- **Related**: [ADR-0008](0008-push-based-supply-event-delivery.md) (push-based delivery through
  `ReactWaker` sinks; this ADR adds a completion handshake on top of it),
  [ADR-0010](0010-cross-thread-lexical-sharing-scope.md)

## 1. Context

In Rakudo a live `Supplier` delivers synchronously: `emit` calls every tap's callback on the
emitting thread before it returns. A `react` taps its `whenever` sources at the moment the
`whenever` runs, and serializes the callbacks with its body and with each other through the
react's lock. So code right after `$supplier.emit(...)` observes what the `whenever` did, even
when the `react` was started on another thread:

```raku
my @got;
my $s = Supplier.new;
my $ready = Promise.new;
start react { whenever $s.Supply -> $m { @got.push($m) }; $ready.keep };
await $ready;
$s.emit($_) for 1..3;
say +@got;        # raku: 3
```

mutsu printed `0`. A react body runs on its own interpreter (`clone_for_thread`), its drive loop
registers the supplier sinks only after the body has finished, and an `emit` from another thread
merely pushed the value into the react's `ReactWaker` queue (ADR-0008) and returned. Every
library that logs, collects or forwards through `start react { whenever $supplier { ... } }` and
then checks the effect (Log::Dispatch's whole suite) saw nothing yet.

## 2. Options

1. **Run the `whenever` body on the emitting thread** (Rakudo's model literally). The body is a
   closure over the react's interpreter state: its lexicals, its `done`, its LAST/QUIT phasers,
   the subscriptions the drive loop owns. Running it on the producer's interpreter would split
   that state across threads and need the react's whole drive state to become shareable and
   locked. Rejected: a rewrite of the react machinery for no observable gain over option 2.
2. **The producer waits until the react has handled its event.** The handler stays on the
   react's own interpreter, the react's single thread is its serialization (what Rakudo's lock
   gives), and the producer returns at the same point in time Rakudo's would. Chosen.
3. **Leave delivery asynchronous** and document it. Rejected: it is a compatibility gap that real
   distributions hit, and every `sleep` a test needs to paper over it is a flaky test.

## 3. Decision

A producer of a live supplier event (`emit`, `done`, `quit`) returns only after every `react`
on *another* thread that taps the supplier has handled that event, or has ended.

- **Completion handshake on the waker.** While a drive loop runs it marks its `ReactWaker`
  synchronous (`SynchronousDelivery`, recording the consuming thread). `drain()` moves the
  drained sequences *in flight*; the dispatcher releases them (`finish_in_flight`) after
  handling the batch, and on every exit through a guard. A producer pushes its event under the
  supplier registry lock as before (ADR-0008 ordering is unchanged), then — outside the lock —
  waits on the waker's condvar until its sequence is neither queued nor in flight
  (`ReactWaker::await_delivery`), GC-quiescent like every other blocking wait.
- **Setup holds.** Rakudo taps at the `whenever`; mutsu registers sinks after the body. To close
  that window the first `whenever` on a live supplier in a react body creates the react's waker
  (`runtime/react_setup.rs`) and records it on the supplier as a *setup hold*. A producer that
  emits meanwhile waits on the hold; the drive loop registers its sinks on that same waker (the
  registration replays the buffered event with its original sequence), starts synchronous
  delivery, and the producer then waits for the handler like any later one.
- **Never a deadlock.** A consumer that blocks while handling events — an `await`, a `Lock`, a
  nested react's idle wait, its own synchronous `emit` into a react that waits on it — cannot
  reach the queued event, while Rakudo would have run it anyway (the react lock is released
  across an `await`, and same-thread re-entry is queued). Every blocking point already passes
  through `worker_pool::enter_blocking`; it now *parks* the wakers whose events the thread is
  handling (`waker::park_dispatching`), and a producer stops waiting for a parked consumer. The
  event stays queued and is handled asynchronously, as before this ADR. The same rule covers a
  react body that blocks while its setup holds are live. A producer on the consuming thread
  itself never waits: the drive loop re-drains what its handlers emit.
- **Visibility.** After a synchronous delivery the producer pulls the handler's writes to
  shared variables (`sync_shared_vars_to_env`), as returning from an `await` does, so
  `@got` above reads `3` without a synchronization point of its own.

Only react drive loops consume synchronously. `await $supply`, `.list`, throttle control waits
and the other collect-only sinks stay asynchronous: they run no user code per event.

## 4. Consequences

- The issue's snippet prints `3`; Log::Dispatch `t/010`, `t/030` and the first subtest of `t/040`
  pass. `t/020` fails for a different reason, filed as [#11318](https://github.com/tokuhirom/mutsu/issues/11318): a `$PROCESS::OUT` assignment is
  not seen by a thread that was already running.
- A producer now pays the handler's run time, as in Rakudo. A slow `whenever` throttles its
  producers instead of letting an unbounded queue build up.
- Divergence kept: Rakudo drops a live supplier's emissions made before any tap exists; mutsu
  still buffers and replays them (ADR-0008 §Consequences), now delivered synchronously when a
  setup hold or sink exists at emit time and asynchronously otherwise.
- Divergence kept: a handler that blocks (or a `sleep` in it) releases its producers early,
  where Rakudo's producer would wait for the handler to finish. This is the price of running the
  handler on the react's thread, and it only ever moves towards the old asynchronous behavior.
- Pinned by `t/concurrency/supply/react-supplier-emit-synchronous.t`; S17 roast is the
  regression net.
