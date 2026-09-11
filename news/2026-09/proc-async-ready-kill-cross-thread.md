# Proc::Async `.ready` and `.kill` work from a thread that did not call `.start`

`roast/S17-procasync/kill.t` uses this shape twice:

```raku
start { await $p.ready; $p.kill }
await $p.start
```

Under mutsu neither half worked, and the file only stayed green because its
children were spawned as `$*EXECUTABLE, "-e", "sleep"` — and a bare `sleep` fell
through the parser as a bareword, so the child exited in 20ms on its own,
whatever the parent did. Fixing that parse (see
`paren-less-zero-arg-builtin-call.md`) made the children block and exposed both
bugs at once as a hard deadlock.

## `.ready` handed out a promise nobody held

The `ready` handler built a fresh `SharedPromise` and stored it into the
instance attributes as `ready_promise`, expecting `.start` to keep it. But the
handler stores through *its own copy* of those attributes, so the write is only
visible to whoever runs next on that thread. With `.ready` called from a `start`
block and `.start` from the parent, `.start` saw no `ready_promise` and kept
nothing; `await $p.ready` then waited forever on a promise with no other
reference to it.

The promise is now created by the constructor
(`build_native_proc_async_value`), so `.ready` and `.start` share one promise no
matter which thread reaches which first, and `.ready` only has to hand it back.
A `Proc::Async` built by some other path still falls back to the old lazy
construction.

## `.kill` reported a running process as not started

`.kill` needs two facts — `started` and `pid` — and `.start` writes both through
the same per-thread copy. From another thread, `.kill` therefore saw
`started = False` and raised `X::Proc::Async::MustBeStarted` against a process
that was demonstrably running.

The ready promise is shared by reference and `.start` keeps it *with the pid*,
so it is the one piece of cross-thread truth already available: `.kill` now
takes the pid from it when the instance attributes do not carry one, and treats
a kept ready promise as proof the process started.

This is a targeted fix for `Proc::Async`, not a general one. Instance-attribute
mutations still do not propagate across threads — `.write`, `.close-stdin` and
the other `MustBeStarted` gates have the same latent problem, and the real
answer is for a `Proc::Async`'s live process state to live outside the
per-thread attribute copies altogether.

Pinned by `t/concurrency/thread-lock/proc-async-ready-cross-thread-kill.t`, which checks
that `.kill` before `.start` is still `X::Proc::Async::MustBeStarted`, that
`.ready` resolves with the pid on the starting thread, and that
`start { await $p.ready; $p.kill }` ends a child blocked in a bare `sleep` —
which it can only do if both halves work cross-thread.
