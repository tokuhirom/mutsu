# Pipe blocks: `native-descriptor`, `$*OUT = $handle` and user `READ` handles

Three gaps behind Shell::DSL's `pb({ ... })` pipe blocks (its `t/01-basic.t`
now passes the 15 assertions rakudo passes before its own EAGAIN death):

- `Proc::Async::Pipe.native-descriptor` answered a placeholder `1` / `2`, so a
  caller reading the descriptor (Shell::DSL's `IO::PipeReadFD`) read the wrong
  stream. A `Proc::Async` stream whose descriptor was asked for, or that was
  selected with `.stdout` / `.stderr` and has no tap when the process spawns,
  is no longer read by the runtime: its pipe end is handed over as a real
  (close-on-exec) descriptor, which the promise carries and the caller closes.
- `$*OUT = $handle` after `my $*OUT = ...` (and the same for `$*ERR` / `$*IN`)
  did not redirect `say` / `put` / `print` inside a routine: the assignment
  never reached the `$*OUT` env twin the output routines read
  (`flush_local_to_env`), and the handle sat in a Scalar cell the output
  dispatch did not look through (`write_to_named_handle`).
- `lines` on a user `IO::Handle` subclass with `READ` yielded an extra empty
  line after a trailing newline when the handle only learns of the end from an
  empty `READ` (`read_user_io_line`).
