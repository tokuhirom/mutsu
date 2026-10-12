# ADR-11502: The Asynchronous `nqp::` ops push onto a blocking queue

- **Status**: Proposed (2026-10-12). §2.1–§2.3 are implemented in the PR that adds
  `runtime/nqp_ops_async.rs`; §2.4 is not yet implemented.
- **Date**: 2026-10-12
- **Issue**: [#11502](https://github.com/tokuhirom/mutsu/issues/11502) (campaign
  [#11488](https://github.com/tokuhirom/mutsu/issues/11488))
- **Related**: [ADR-0020](0020-shared-worker-pool.md) (the worker pool and the
  deadline-heap timer)

## 1. Question

Most of #11502 is done: Atomic in #11819 and Threads in #11830. What is left is the
Asynchronous family: `timer`, `cancel`, `signal`, `permit`, `spawnprocasync`,
`killprocasync`, `asyncconnect`, `asynclisten`, `asyncreadbytes`, `asyncwritebytes` and
`watchfile`.

In MoarVM these ops all have the same shape. An op takes:

- a queue of REPR `ConcBlockingQueue`;
- a schedulee;
- arguments of its own;
- a handle type of REPR `AsyncTask`.

It returns a cancellable handle of that type. When the event happens, the schedulee (or a
list of it and the event's arguments) is pushed onto the queue, and whoever `shift`s the
queue picks it up. Rakudo's own scheduler is the main consumer: `$*SCHEDULER.queue` is a
`ThreadPoolScheduler::Queue is repr('ConcBlockingQueue')` drained by worker threads.

mutsu implements the scheduler, `Proc::Async`, `IO::Socket::Async`, `signal` and
`IO::Notification` natively, without these ops. So the question is what the ops are in
mutsu, and how to give them their queue without rebuilding Rakudo's internals.

## 2. Decision

### 2.1 The two REPRs are class traits, as in Rakudo

`is repr('ConcBlockingQueue')` and `is repr('AsyncTask')` are recorded per class (in the
registry's `async_repr_classes`). `.REPR` reports them for the type object and for an
instance. `nqp::create` allocates storage for each:

- **A queue** is an instance whose storage is a `SharedChannel`. `nqp::push` sends to it,
  `nqp::shift` blocks quiescently until an item arrives, and `nqp::elems` counts the items
  waiting. Any other positional op raises Rakudo's `This representation
  (ConcBlockingQueue) does not support positional access (for type …)`.
- **A task** is an instance holding a cancellation id. The id comes from the registry
  `Cancellation` already uses, so both kinds of handle share one cancel mechanism.

The storage sits under a bare attribute name (`queue`, `task`), as a Buf keeps its `bytes`,
so no declared `$!attr` can collide with it.

### 2.2 `nqp::timer` is an entry on the shared timer heap

`nqp::timer($q, $schedulee, $timeout_ms, $repeat_ms, $Type)` validates its operands with
Rakudo's errors:

- `timer result type must have REPR AsyncTask`;
- `timer target queue must have ConcBlockingQueue REPR`.

It then registers one entry on the deadline heap of `interval_timer` (ADR-0020). The
entry's action only sends the schedulee into the channel. That is cheap, and it runs no VM
code on the timer thread. The action repeats every `$repeat_ms` when that is non-zero, and
stops once the task's flag is set. The op answers a new `$Type` handle.

### 2.3 `nqp::cancel` sets the task's flag

It answers the handle. Cancelling twice is harmless.

### 2.4 `signal`, the process ops, the socket ops, `permit` and `watchfile` (not yet implemented)

Their native implementations end in a supply channel (`SupplySender::send`):

- `signal_watcher.rs`;
- `native_proc_async.rs`;
- `native_methods/socket_async*.rs`;
- `native_io/io_notification.rs`.

To serve these ops, each one gets a sink that can also be a queue push of `[schedulee,
args…]`, behind one abstraction. Each op then reuses its native implementation; none gets
a second one. That refactor is the next slice.

### 2.5 Not provided: `$*SCHEDULER.queue`

`ThreadPoolScheduler.queue` is an undocumented Rakudo internal. A program that wants these
ops can declare its own queue class, as above. Pushing onto mutsu's scheduler would need
the worker pool to accept arbitrary queue items, which nothing in the ecosystem asks for:
none of the 1641 distributions in the ecosystem ledger fails on a missing op of this
family.

## 3. Consequences

- Rakudo's own way of building these ops works: a queue class, a handle class, and
  `nqp::shift` to wait on events.
- The timer adds no thread: it shares the heap `Supply.interval` and `Promise.in` use.
- `docs/nqp-op-coverage.md`: Asynchronous goes from 0 / 11 to 2 / 11.
