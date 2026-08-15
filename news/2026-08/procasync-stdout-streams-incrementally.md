# `Proc::Async` stdout/stderr taps registered before `.start()` now stream incrementally

A tap on `Proc::Async`'s `.stdout`/`.stderr` used to receive nothing until the
child process exited, and then the whole run's output as a single chunk —
any handshake where the parent has to read something (e.g. an ephemeral port
number) from a still-running child deadlocked, or needed a file-based
workaround (`t/io-socket-async-real-connect.t`).

The reader thread `.start()` spawns already streamed each chunk onto the
output Supply's channel as it was read (`native_proc_async.rs`) — nothing
drained that channel until the child exited. A tap registered before
`.start()` — the only order `X::Proc::Async::TapBeforeSpawn` allows — now
drains it live, the same act-loop pump every other channel-backed Supply
(signals, sockets, …) already uses, joined before the `.start()` Promise
settles so `await`/`.result` still observes the very last chunk.

Two gaps the live path's own decode had to fix on the way, both invisible in
the old batch-at-exit delivery: the raw byte stream's `\r\n` → `\n`
translation (mutsu-specific, stdout only) now holds back a trailing lone
`\r` across `read()` boundaries instead of only ever seeing it inside a
fully-collected string, and the live path only activates for the default
UTF-8 encoding — a non-default `:enc` (rare) still falls back to the
correct, replay-based decode, just not incrementally.

**Scope note:** a tap registered via `whenever` inside a `react`/`supply`
block is deliberately excluded — `whenever` bodies there share lexicals with
their siblings through the react loop's own single-threaded dispatch, not a
general cross-thread cell, so running such a tap's callback on a genuinely
separate OS thread could leave a write invisible to a sibling `whenever`
that reads it (`roast/S17-procasync/basic.t`'s chained-stdin cases pinned
this). Plain `.tap()` outside `react` — the shape this ticket's own repro
used — streams live; `whenever`-registered taps keep the previous
deliver-everything-at-exit behavior, which is still correct, just not
incremental. A tap registered *after* `.start()` also keeps the old
behavior on purpose: `replay_proc_taps` runs synchronously on whichever
thread evaluates `await`/`.result`, which is the only way to guarantee the
tap sees everything by the time that statement returns.

Pinned by `t/procasync-stdout-incremental.t`.
