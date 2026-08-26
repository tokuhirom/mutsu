# The process now waits for outstanding non-`app_lifetime` `Thread`s

`Thread.start({ for 1..10 -> $v { say $v } })` with no `.join` produced **no output at all** under
mutsu: the mainline ran out of statements and `main()` called `std::process::exit`, killing the
spawned thread before it could run. Adding an explicit `.finish` made the same code print, so the
spawn mechanism itself was fine — mutsu simply never waited.

## The rule this had to match (established against raku v2026.06 first)

`Type/Thread.rakudoc` on `:$app_lifetime`: "If `$app_lifetime` is set to `True`, then the thread is
killed when the main thread of the process terminates. If set to `False`, the process will only
terminate when the thread has finished." `False` is the default for both `Thread.new` and
`Thread.start`.

The ticket's premise was worth checking rather than assuming, because a blanket "wait at exit" would
be a hang risk. Probing real raku pinned four separate behaviours, and mutsu now reproduces all four:

| program | raku | mutsu (before) | mutsu (now) |
| --- | --- | --- | --- |
| `Thread.start({ sleep 1; say "T" }); say "main"` | `main`, then `T` | `main` only | `main`, then `T` |
| same with `:app_lifetime` | `main` only | `main` only | `main` only |
| `... ; exit 0` | `main` only | `main` only | `main` only |
| `... ; die "boom"` | error only | error only | error only |
| `END { say "END" }` present | `main`, `END`, `T` | — | `main`, `END`, `T` |

So the wait is specific to **normal completion**, happens **after** the `END` phasers, and skips
`app_lifetime` threads entirely.

## What changed

- `methods_collection_ops::join_outstanding_threads()` drains the `THREAD_HANDLES` registry in id
  order and joins each handle through `gc::block_quiescent`, exactly as `Thread.finish` already did
  (a thread blocked in a join counts as quiescent for the cooperative stop-the-world).
- `Interpreter::join_outstanding_threads()` wraps it with the same post-join synchronization
  `.finish` performs — `sync_shared_vars_to_env()` and `drain_shared_thread_output()` — so a joined
  thread's shared-variable writes and buffered (e.g. subtest-internal TAP) output are not lost.
- `main()` calls it on the `Ok(_)` arm only, and only when `!interpreter.exit_requested()`. The
  `Err` arm (uncaught exception) and `exit` therefore terminate immediately, matching raku.

The registry only ever holds `Thread.start` / `Thread.run` handles — `start { }` / `Promise` /
`Supply` pool workers are not in it — so no fire-and-forget async work newly blocks process exit.
An `:app_lifetime` handle is dropped at spawn and was never in the registry to begin with.

Pinned by `t/thread-and-lock-async.t`, whose last four subtests run each of the shapes above as a
child `$*EXECUTABLE` process and compare its captured stdout; the file passes unchanged under both
`raku` and `mutsu`.
