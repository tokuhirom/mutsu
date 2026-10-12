# `nqp::signal` pushes signals onto a ConcBlockingQueue

`nqp::signal($setup_queue, $setup_schedulee, $queue, $schedulee, $signum, $type)`
now works as in MoarVM (#11502, ADR-11502 §2.4.1):

- Once the handler is installed, `[$setup_schedulee]` is pushed onto the setup queue.
- Each delivery of `$signum` pushes `[$schedulee, $signum]` onto `$queue`. The
  signal number is a `BOOTInt`.
- The op answers a handle of the `is repr('AsyncTask')` class it is given.
  `nqp::cancel` on the last handle for a signal restores the signal's previous
  disposition.
- Bad operands get Rakudo's errors, including `Unsupported signal handler N`.

The op does not get a second signal mechanism. The self-pipe watcher behind
`signal()` now holds two kinds of registration: the Supply it always fed, and a
`QueueSink` (queue, schedulee, cancellation flag). The rest of ADR-11502 §2.4
(process, socket, `permit` and `watchfile` ops) is to reuse that sink. The
watcher's Unix implementation and its tests moved to their own files to keep each
file under 500 lines.

`nqp::` coverage: 561 / 575.
