# Worker-pool probes and the recursive-`start` corruption repro

Reproducible harness behind the two PLAN §6 entries (the shared-worker-pool
ADR groundwork, and the silent wrong answer in recursion through `start`).
Measured 2026-07-17 on release `main` 159a30cb0, 12 cores, raku 2026.06 on the
same host. Run every probe against both implementations:

```
cargo build --release
timeout 180 target/release/mutsu docs/probes/<file>.raku
timeout 180 raku                  docs/probes/<file>.raku
```

The scripts live in `docs/probes/`. They are deliberately *not* under `t/` —
`pool-recursive-start.raku` currently fails, and `t/` is fatal in CI. Promote
it to a `t/` pin as part of the fix.

## Results

| probe | mutsu | raku |
|---|---|---|
| `pool-spawn.raku 500` — 500 × trivial `start {}` | 0.232–0.262s | 0.051–0.07s |
| `pool-idle-cue.raku 50` — 50 idle `cue(:every(60))` | RSS +20.7 MB, **VmSize +16.4 GB**, threads 2→**52** | RSS +4.3 MB, VmSize +25 MB, threads 2→**5** |
| `pool-blocking.raku 200` — 200 × `start { sleep 2 }` | 2.09s (unbounded concurrency) | 6.1s (bounded at `max_threads`, 3 batches) |
| `pool-nested-await.raku 500` — nested `start`+`await`, depth 500 | 0.99s (500 real OS threads) | **0.12s** |
| `pool-recursive-start.raku` — recursion through `start` | **wrong answers, then hangs** | correct |

## What the pool probes establish

`raku -e 'say $*SCHEDULER.max_threads'` prints **96** here (8 × cpu-cores).
Two facts that look contradictory until you account for continuations:

- Genuinely-blocking tasks *do* serialize against that bound —
  200 × `sleep 2` takes 6.1s, not 2s, i.e. three batches of ~96.
- Yet depth-500 nested `await` completes in 0.12s on those same 96 workers.

Rakudo's `await` on a pool thread yields a MoarVM continuation (`$*AWAITER`)
and hands the worker back rather than blocking it. **mutsu has no
continuations, so a bounded pool plus a blocking `await` deadlocks** — the
depth-500 shape would pin every worker. That, not pool sizing, is the ADR's
central question.

## What the corruption repro establishes

`pool-recursive-start.raku` collapses every frame's `$n` to one value. Plain
recursion (no `start`) and non-recursive `start` are both correct, and so are
*sequential* same-name frames — `sub a($x)` / `sub b($x)` interleaved, and
`p(1); p(2); p(3)`, all match raku. The corruption needs same-name frames
**simultaneously live across a thread boundary**, which is what
`clone_for_thread`'s flat name-keyed `shared_vars` map
(`src/runtime/runtime_thread.rs:18-53`) cannot represent.
