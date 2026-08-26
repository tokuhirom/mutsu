# `Thread.new(...).run` now starts the thread

`Thread.new(code => { ... })` builds a `Thread` **without** starting it; `.run` is the method that
hands its code to an OS thread and returns the invocant. mutsu had no `.run` at all, and its
`Thread.new` was the generic "stash the named arguments as attributes" constructor — so the
resulting object carried a `code` attribute nobody could ever reach, and no `id` (`$t.id` answered
`0`, and two `Thread.new`s were indistinguishable).

`Thread.start` was implemented as one monolithic "allocate an id, clone the interpreter, spawn"
routine, so there was no spawn step for `.run` to reuse.

## What changed

- `Thread.new` is now a real native constructor (`dispatch_new`'s `"Thread"` arm): it requires
  `:&code`, defaults `:$name` to `'<anon>'` and `:$app_lifetime` to `False`, and **allocates the
  thread id up front** — rakudo reports a real, unique `.id` on a not-yet-started `Thread`, verified
  against raku v2026.06.
- The spawn half of `Thread.start` was extracted into `spawn_thread_body`, and `Thread.run` (new,
  `dispatch_thread_run`) drives it from the instance's own attributes, returning the invocant so
  `$t.run.finish` chains the way rakudo's `method run(Thread:D:)` does.
- A process-global `STARTED_THREADS` set makes "it is an error to run a thread that has already been
  started" observable: rakudo enforces it with a MoarVM-level panic (`Invalid GC status observed "2"
  while blocking thread; aborting` — reproduced on v2026.06), mutsu raises an ordinary catchable
  exception instead.
- `Thread.Numeric` was added alongside `.id` (the documented alias), and `Thread`'s
  hardcoded-native-method list was extended so it dispatches instead of falling through to `Mu`'s
  "uninitialized value in numeric context" path.
- The `Thread` dispatchers moved out of `methods_collection_ops/socket_thread.rs` into a new
  `thread_ops.rs`, keeping both files under the 500-line limit.

Pinned by `t/thread-and-lock-async.t`, which asserts the same expectations under both `raku` and
`mutsu`.
