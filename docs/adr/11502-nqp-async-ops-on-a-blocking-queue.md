# ADR-11502: The Asynchronous `nqp::` ops push onto a blocking queue

- **Status**: Accepted (user decision 2026-10-12; Proposed the same day). §2.1–§2.3 are implemented in the PR that adds
  `runtime/nqp_ops_async.rs`. Of §2.4, `signal` (§2.4.1), `watchfile` (§2.4.2) and
  the process ops `spawnprocasync`, `permit` and `killprocasync` (§2.4.3) are
  implemented; the socket ops are not yet.
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

#### 2.4.1 `nqp::signal` (implemented)

MoarVM's operand order is `nqp::signal($setup_queue, $setup_schedulee, $queue,
$schedulee, $signum, $type)`. Rakudo's `signal()` waits on the setup queue so that the
handler is armed before it returns. Observed on Rakudo 2026.09:

- `[$setup_schedulee]` is pushed onto the setup queue once the handler is installed.
- Each delivery pushes `[$schedulee, $signum]`, with `$signum` a `BOOTInt`.
- Each handle on a signal gets its own push.
- Cancelling the last handle gives the signal back to its previous disposition.
- Errors, in the order they are checked:
  - `signal result type must have REPR AsyncTask`;
  - `signal target queue must have ConcBlockingQueue REPR`;
  - `signal setup notify queue must have ConcBlockingQueue REPR`;
  - `Unsupported signal handler N` for a number outside the standard signals.

mutsu adds the sink as a second kind of registration in the existing signal watcher
(`signal_watcher_unix.rs`). A registration is either the `signal()` Supply it always
served, or a `QueueSink` (`nqp_ops_async.rs`): the queue, the schedulee and the task's
cancellation flag. They share one registry, one self-pipe handler and one
disposition restore. A queue registration retires when its flag is set, and
`nqp::cancel` runs the watcher's retire sweep at once, so the disposition goes back at
the cancel, as in MoarVM. The other §2.4 ops are to reuse `QueueSink` the same way.

#### 2.4.2 `nqp::watchfile` (implemented)

`nqp::watchfile($queue, $schedulee, $path, $type)`. Observed on Rakudo 2026.09:

- Each change pushes `[$schedulee, $name, $renamed, $error]`:
  - `$name` is a `BOOTStr`: the entry name for a watched directory, the basename for a
    watched file;
  - `$renamed` is a `BOOTInt`, 1 for an entry appearing or disappearing and 0 for a change;
  - `$error` is the `BOOTStr` type object.
- A missing path is not an error at the call. It pushes the `BOOTStr` and `BOOTInt` type
  objects, then `no such file or directory`.
- Errors:
  - `file watch result type must have REPR AsyncTask`;
  - `file watch target queue must have ConcBlockingQueue REPR`.

mutsu runs the same poller as `IO::Notification.watch-path` (`native_io/io_notification.rs`).
That poller now reports to a `WatchSink`, which is either the Supply of `watch-path` or a
`QueueSink`. A Supply sink starts reporting at its first tap, as before. A queue sink
starts reporting at once, and the poller ends when its task is cancelled. Event kinds come
from the poller's snapshot diff, so they follow the same mapping as `watch-path`, not
libuv's inotify mapping. For example, unlinking a watched file is "renamed" in mutsu, but
"changed" under inotify.

#### 2.4.3 `nqp::spawnprocasync`, `nqp::permit` and `nqp::killprocasync` (implemented)

`nqp::spawnprocasync($queue, $program, $argv, $cwd, $env, $callbacks)` takes no handle
type. Its handle is a `BOOTIO` (REPR `MVMOSHandle`). `$argv` starts with the program's own
name, and `$callbacks` is a hash: each event pushes `[callback, args…]` with the callback
taken from it. Observed on Rakudo 2026.09:

- `ready`: `[ready, BOOTIntArray[stdout fd, stderr fd], BOOTInt pid]`. A fd is -1 for a
  stream without a `*_bytes` callback, which is not piped and goes to the parent's own.
- `stdout_bytes` / `stderr_bytes`: `[cb, BOOTInt seq, Buf, BOOTStr]` per chunk, where the
  `Buf` is of `buf_type`'s class and the last slot is the `BOOTStr` type object. The
  end of the stream is `[cb, seq, BOOTStr, BOOTStr]`, and a read error goes in the last
  slot. `seq` counts the stream's events from 0.
- A stream is read only after `nqp::permit($handle, $channel, $permits)`: channel 1 is
  stdout, 2 is stderr. `$permits` is how many more chunks may be read: -1 for any number,
  and 0 pauses the stream. `permit` answers the handle, and dies with `Can only permit
  an AsyncTask handle` for another object.
- `done`: `[done, BOOTInt status]`, the raw wait status: `exit << 8`, or the signal
  number.
- A failed spawn pushes `[error, BOOTStr message, BOOTInt -errno]` and no `ready`. The
  message is `Failed to spawn process <program>: <libuv text> (error code <-errno>)`. The
  op still answers a handle.
- stdin is closed unless `write` is present.
- `nqp::killprocasync($handle, $signal)` signals the child and answers 0. Killing a reaped
  child is not an error; another object dies with `killprocasync requires a process
  handle`.

Rakudo orders `done` against the output only loosely (MoarVM pushes `done` when libuv
reports the exit). mutsu pushes `done` after every *permitted* reader has nothing more to
push for now: it has reached its end, or it waits for a permit. A permit given after the
child was reaped still reads what the pipe holds, after `done`.

mutsu spawns the child and reads its pipes with `Proc::Async.start`'s code
(`runtime/proc_spawn.rs`: `ProcSpawn` and `pump_pipe`). Only what happens to a chunk
differs: `Proc::Async` decodes it into its Supplies, and the op pushes a `Buf` onto the
queue. Not modelled yet, and refused with an error that names the option: `merge_bytes`
(permit channel 0), `pty`, and binding the child's stdio to a given fd (`stdin_fd`,
`stdout_fd`, `stderr_fd`). The write side (`write`, then `asyncwritebytes` on the handle)
belongs with the socket ops.

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
- `docs/nqp-op-coverage.md`: Asynchronous goes from 0 / 11 to 2 / 11 (§2.1–§2.3), then
  to 3 / 11 (`signal`), then to 4 / 11 (`watchfile`), then to 7 / 11 (the process ops).
