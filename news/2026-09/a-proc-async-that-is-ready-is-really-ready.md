# A Proc::Async that is ready is really ready

`roast/S17-procasync/kill.t` had been failing about once in eighty runs, on two
different CI jobs and two unrelated diffs. It was filed as a suspected flake
([#7929](https://github.com/tokuhirom/mutsu/issues/7929)) rather than quarantined,
because `docs/flaky-test-policy.md` wants evidence first and because the two S17
flakes that were previously root-caused — `S17-supply/batch.t` and `t/lock.t` —
both turned out to be deterministic logic bugs. This one did too.

The failing subtest does not hang. It dies:

```
    not ok 3 - STDERR
    #      got: "Unhandled exception in code scheduled on thread 7\n
    #             X::Proc::Async::MustBeStarted()\n  in block <unit> at -e line 5\n"
```

`MustBeStarted`, thrown by a `.kill` that ran *after* `await $p.ready` had
already returned:

```raku
start {
    await $p.ready;                        # returns...
    $n == 3 ?? $p.kill !! $p.kill: (...)   # ...and this says "must be started"
}
await $p.start
```

## The snapshot convention

A native *mutable* instance method is handed an owned **clone** of the
instance's attribute map and returns a new one, which the caller commits
afterwards (`call_native_instance_method_mut` → `Value::write_back_sharing` →
`commit_attrs`). Nothing a method writes is visible to another thread until it
has returned and the caller has published the map.

`start` writes `started` and `pid` into its copy, spawns, and then resolves
`.ready` — all before it returns. The thread that `.ready` wakes reads
`started` out of the instance, where the commit has not landed, and throws.

The same convention broke the other half of the handshake even harder. `.ready`
used to *mint* its promise on demand and hand it to `.start` through the map. A
`.ready` that raced `.start` built that promise from a pre-spawn snapshot;
`.start`, already past its own resolve point and holding an older snapshot,
never saw it and never kept it. `await $p.ready` then hung forever — measured at
roughly three runs in four, far more often than the roast failure that led here.

## The fix

The promise is built by the constructor now, and `.start` keeps it with the pid
as soon as `spawn()` succeeds. A promise is shared by reference rather than
copied with the map, so it is the same object in every snapshot however stale,
and `keep` publishes the status and the value under one lock before waking
anyone. `kill`, `write`, `say`/`put`/`print` and `close-stdin` read "has it
spawned, and as what pid" from that latch (`proc_async_spawned_pid`), falling
back to it only when their own snapshot has no `pid` — so the un-started case
still throws `MustBeStarted`, as `roast/S17-procasync/basic.t` requires. A third
site is fixed for free: `Proc::Async.pid` used to hand back a freshly minted,
never-kept promise when it found neither a pid nor a `ready_promise`.

## Measurements

`roast/S17-procasync/kill.t`, release build, under the `gc-stress` environment:

| | runs | failures |
|---|---|---|
| before, idle box | 140 | 1 |
| before, `flake-repro.sh -l 4` | 40 | 1 |
| after, `flake-repro.sh -l 4` | 60 | 0 |

Nothing was added to `flaky-tests.txt`: the answer to "flaky or broken" was
broken.

Pinned by `t/concurrency/procasync-ready-latch.t`, which fails 6 times out of 6
against a binary without this change and passes 8/8 with it, in 0.4s. All six
assertions were verified against rakudo.

## What this does not fix

The snapshot-and-commit convention itself is still there, and it has a second
consequence this change only sidesteps: because `commit_attrs` replaces the map
wholesale, a method that started from an older snapshot commits it over a newer
one and silently discards another thread's writes. That is the instance-attribute
twin of the `env` clobber fixed in #4167, it touches every native mut handler,
and it is filed as [#7943](https://github.com/tokuhirom/mutsu/issues/7943) for a
design pass rather than patched here.

Root-causing this also turned up
[#7942](https://github.com/tokuhirom/mutsu/issues/7942): a bare `sleep` — no
parens, no argument — parses as `BareWord("sleep")` and never calls the routine,
where rakudo sleeps forever (`sub sleep($seconds = Inf)`). `kill.t` spawns its
children as `-e "sleep"`, so under mutsu they exit immediately and several of its
subtests are signalling processes that have already gone. The file passes either
way, but it is not testing what it reads as.
