# `roast/S17-supply/syntax.t` takes ~57s wall and ~610 CPU-seconds

The single file `roast/S17-supply/syntax.t` (90 tests) runs for **57 seconds
wall clock** on a release build, consuming **~610 CPU-seconds** — roughly 11
cores' worth of work for one supply-syntax test file. `raku` runs it in well
under a second.

```
$ MUTSU_FUDGE=1 prove -e 'target/release/mutsu' roast/S17-supply/syntax.t
All tests successful.
Files=1, Tests=90, 57 wallclock secs ( 0.02 usr 0.01 sys + 554.92 cusr 55.98 csys = 610.93 CPU)
```

The CPU/wall ratio says the time is spent spinning, not computing: something in
the supply/react runtime is busy-waiting rather than blocking on a condvar.

## Why it matters

It is the whole margin of the roast budget. CI run 31191459333's `jit-stress`
job failed with

```
roast/S17-supply/syntax.t   Dubious, test returned 124 (wstat 31744)
  Parse errors: Bad plan.  You planned 90 tests but ran 70.
```

— the timeout shape, on a file that needs 57 of its allotted seconds even on an
idle machine. Under the parallel load of a full roast sweep it does not fit.
This will keep producing "flaky" CI reds until the spin is fixed; quarantining
it in `flaky-tests.txt` would only hide a real performance defect.

## Not a regression

Measured on `75b0ad4ca` (before the supply emitter-stamp work of #6044/#6047):
identical 57s / 610 CPU-s. The slowness predates that campaign.

## Where to look

`perf record` the file and look for the busy loop. Likely candidates are the
react drive loop's polling (`vm/vm_react_loop.rs`, `vm_react_subscriptions.rs`)
and `Self::sleep_for_supply_delay` / the promise-wait paths, any of which
spinning would explain a ~11:1 CPU-to-wall ratio.

## Deep-dive investigation (2026-08-10)

Profiled on a `--profile profiling` build (release-optimized + debuginfo),
current `main` (52f217429). **The ticket's guesses were wrong**: the react
drive loop (`vm_react_subscriptions.rs`) blocks correctly on its `ReactWaker`
with a 250 ms cap, `sleep_for_supply_delay` is a real blocking sleep, the
worker pool parks on a condvar, and the interval timer is already a single
deadline-heap thread. None of them spins.

### Measured numbers

- Full file, profiling build: **76 s wall, 542 s user + 65 s sys = 607 CPU-s**
  (`timeout 120 env MUTSU_FUDGE=1 target/profiling/mutsu roast/S17-supply/syntax.t`).
- Per-subtest gaps (timestamped TAP output): test 63 "CLOSE phaser sees correct
  outer scope" 6.9 s; test 69 12.1 s; test 70 12.2 s; test 71 30.4 s.
- **Tests 69–71 run in under 0.6 s each when extracted and run in isolation.**
  Their slowness in-file is entirely collateral damage from test 63.

### The root cause: `Tap.close` never tears down the channel-backed act-loop worker

Test 63 (`roast/S17-supply/syntax.t:457-476`) does
`(supply { whenever Supply.interval(0.01) { }; CLOSE {...} }).tap.close`
4000 times. Each `.tap` leaks a pool worker that runs forever:

1. `Supply.interval` (`src/runtime/methods_instance_ops.rs:1508-1583`) creates
   a `supply_event_channel()` pair, parks the `SupplyReceiver` in the
   supply-channel map under a fresh `supply_id`, and registers the
   `SupplySender` on the shared interval-timer heap
   (`src/runtime/native_methods/interval_timer.rs:193-211`). The timer entry
   dies **only when `tx.send` fails**, i.e. when the receiver is dropped.
2. Tapping the enclosing `supply { }` block hits the "live channel-backed
   whenever source" branch of the tap handler
   (`src/runtime/native_supply_mut_methods.rs:527-602`): it
   `take_supply_channel(chan_sid)`s the receiver and
   `worker_pool::submit`s `run_supply_act_loop(driver, rx, body_cb, ...)`
   (submit at line 593).
3. `run_supply_act_loop` (`src/runtime/native_methods/encoding.rs:444-541`)
   blocks in `rx.recv()` and dispatches the `whenever` body via
   `call_sub_value` for every tick. It exits only on channel disconnect,
   `Done`/`Quit` with no handler, or a `done`-family signal raised by the body.
4. `Tap.close` (`native_tap` "close",
   `src/runtime/native_methods/scheduler.rs:167-225`) closes supplier-registry
   taps, cascades `upstream_taps`, and fires CLOSE phasers — but **the
   channel-backed branch records nothing in `upstream_taps` and nothing in the
   Tap handle that could reach the worker**. The receiver is owned by the
   blocked worker; the sender is owned by the timer heap; neither ever drops.
   `SupplyReceiver`/`SupplySender`
   (`src/runtime/native_methods/supply_channel.rs`) have **no receiver-side
   close** at all.

So after `.close` the timer keeps sending a tick every 10 ms and the worker
keeps waking and dispatching the (empty) `whenever` body through the full
interpreter path — forever, until process exit. 4000 taps → ~4000 permanent
workers × 100 dispatches/s ≈ 400 K interpreter dispatches/s across all cores.

**Empirical confirmation** (repro below, taps all closed, then `sleep 10`):
the process held **3379 live threads at 956 % CPU** during the sleep, and the
run's CPU total grew from 77 CPU-s (no sleep) to 113 CPU-s (+36 CPU-s burned
by "idle" leaked workers in ~10 s). The direct-tap path
(`Supply.interval(0.01).tap({;}).close`, handler branch at
`native_supply_mut_methods.rs:236-251`, which returns an **empty** Tap handle)
leaks identically: 100 closed taps still burn ~0.5 core.

### Profile top self-time (perf, `--no-children`)

Whole file (123 K samples): `nanbox::gc_op` 20.5 %, `Gc::drop` 11.9 %,
`nanbox::payload_op` 7.1 %, `LocalKey::with` 4.2 %, `value_eq` 3.1 %, `malloc`
2.6 %, `call_sub_value` 2.2 % — i.e. Value refcount/env churn from the leaked
workers' body dispatches, not a syscall spin. Leak-phase-only attach profile
(after all taps closed) shows the same churn plus the timer thread's fan-out:
`supply_channel::notify_all`, `mpmc Channel::send`, `SyncWaker::notify`,
`futex_wake`, `native_queued_spin_lock_slowpath`. There is a **single** spin
site (the leaked act-loop workers, ~90 % of the burn); the timer-thread send
fan-out is secondary and disappears with the same fix.

### Minimal repro (put in `tmp/`, run with `timeout 60 target/release/mutsu`)

```raku
my atomicint $total = 0;
sub test-close($val) {
    supply {
        whenever Supply.interval(0.01) { }
        CLOSE { $total ⚛+= $val; }
    }
}
await do for ^4 { start for ^1000 { test-close($_).tap.close; } }
say "taps closed, total=$total";
sleep 10;   # CPU should be ~0 here; actually burns ~3.5 cores (3379 threads)
```

Watch `grep Threads /proc/<pid>/status` during the sleep: thousands of leaked
workers.

### Fix plan (step by step)

The wait protocol to build: `Tap.close` sets a per-worker close flag; the act
loop's blocking receive becomes a **bounded** wait (250 ms cap, the
`REACT_IDLE_WAIT` idiom — a safety net, not a latency bound) that re-checks the
flag; the sender also checks the flag so the interval-timer entry dies on its
next tick. No condvar hand-off is needed because the wait is bounded — a missed
wakeup costs at most 250 ms, never a hang.

1. **`src/runtime/native_methods/supply_channel.rs`** — add a shared
   `closed: Arc<AtomicBool>` to `SupplySender` and `SupplyReceiver`, created in
   `supply_event_channel()` (clone into both halves; `SupplySender::clone`
   shares it).
   - `SupplySender::send`: if `self.closed.load(Ordering::Acquire)`, return
     `Err(mpsc::SendError(event))` without sending. (The interval timer treats
     a failed send as "receiver gone" and drops its heap entry —
     `interval_timer.rs:202-208` — exactly the cascade wanted.)
   - Add `SupplyReceiver::recv_timeout(&self, d: Duration) ->
     Result<SupplyEvent, mpsc::RecvTimeoutError>` (delegate to
     `self.rx.recv_timeout(d)`).
   - Add `SupplyReceiver::close_flag(&self) -> Arc<AtomicBool>` (clone of the
     shared flag) so the tap site can keep a handle after moving `rx` into the
     worker.
2. **Registry** (mirror the cancellation-map idiom in
   `src/runtime/native_methods/state_lock.rs:41-55`): a
   `OnceLock<Mutex<HashMap<u64, Arc<AtomicBool>>>>` with
   `register_act_loop_close(flag: Arc<AtomicBool>) -> u64` (own `AtomicU64`
   counter), `close_act_loop(id: u64)` (set flag `Ordering::Release`, remove
   entry; no-op on missing id), and `unregister_act_loop_close(id: u64)`
   (remove only — called by the worker on exit so the map cannot grow).
   Put it in `state_lock.rs` or a small new `state` submodule; re-export via
   `native_methods/mod.rs`.
3. **`run_supply_act_loop`** (`src/runtime/native_methods/encoding.rs:444`) —
   add a parameter `close_flag: Option<(u64, Arc<AtomicBool>)>`. Replace the
   `rx.recv()` at line 460 with:
   ```rust
   let recv = loop {
       if let Some((_, f)) = &close_flag
           && f.load(Ordering::Acquire)
       {
           break Err(());          // treated like disconnect: exit silently
       }
       #[cfg(not(target_arch = "wasm32"))]
       match rx.recv_timeout(Duration::from_millis(250)) {
           Ok(ev) => break Ok(ev),
           Err(mpsc::RecvTimeoutError::Timeout) => continue,
           Err(mpsc::RecvTimeoutError::Disconnected) => break Err(()),
       }
       #[cfg(target_arch = "wasm32")]
       break rx.recv().map_err(|_| ());   // wasm: keep the old blocking recv
   };
   ```
   and match on `recv` where the old code matched `rx.recv()`. **Also re-check
   the flag after a successful receive, before dispatching** — this is what
   makes "no body dispatch starts after `close` returns" a hard guarantee (the
   pin test below relies on it). On *every* exit path (all `break`s fall
   through to the function end — add the call there), run
   `unregister_act_loop_close(id)` when `close_flag` is `Some`.
   Do **not** wrap the wait in `gc::block_quiescent`: the received event moves
   a `Gc` Value, and the current code raw-blocks — keep the GC posture
   unchanged.
4. **Wire the two leaking call sites** in
   `src/runtime/native_supply_mut_methods.rs`:
   - Direct live-supply tap (lines 236-251): before `submit`, do
     `let flag = rx.close_flag(); let fid = register_act_loop_close(flag.clone());`
     pass `Some((fid, flag))` into `run_supply_act_loop`, and return the Tap
     with `{"act_loop_close_ids" => Value::array(vec![Value::int(fid as i64)])}`
     instead of the current empty `HashMap`.
   - Channel-backed whenever source (lines 527-602, submit at 593): same
     dance; collect the ids in a `let mut act_loop_close_ids: Vec<Value>`
     declared next to `upstream_taps` (line 279) — one supply block can have
     several such sources — and at the Tap-handle build (lines 1104-1111)
     insert `"act_loop_close_ids"` into `tap_handle_attrs` when non-empty.
5. **`native_tap` "close"** (`src/runtime/native_methods/scheduler.rs:173`):
   after the `close_supplier_tap` call and before/alongside the
   `close_upstream_taps` cascade, read `attributes.get("act_loop_close_ids")`,
   and for each `ValueView::Int(id)` call `close_act_loop(id as u64)`. Nested
   Tap handles inside `upstream_taps` recurse through `native_tap("close")`
   (`close_upstream_taps`, scheduler.rs:136-165) and will close their own ids —
   no extra recursion needed.
6. Teardown then converges from both ends: the flagged sender makes the timer
   entry die on its next tick even before the worker wakes, and the worker
   exits within ≤250 ms, dropping `rx` (which also disconnects any other
   sender clones).

### Pin test (new file `t/supply-tap-close-interval.t`, Write tool)

```raku
use v6;
plan 2;
{
    my atomicint $ticks = 0;
    my $s = supply { whenever Supply.interval(0.01) { $ticks⚛++ } };
    my $tap = $s.tap({ ; });
    sleep 0.1;
    $tap.close;
    sleep 0.35;            # > one 250ms bounded-wait round + in-flight body
    my $after = ⚛$ticks;
    sleep 0.3;
    is ⚛$ticks, $after, 'supply-block interval stops ticking after tap.close';
}
{
    my atomicint $n = 0;
    my $tap = Supply.interval(0.01).tap({ $n⚛++ });
    sleep 0.1;
    $tap.close;
    sleep 0.35;
    my $after = ⚛$n;
    sleep 0.3;
    is ⚛$n, $after, 'direct interval tap stops ticking after tap.close';
}
```

This is deterministic (not load-flaky) *because of* the post-receive flag
re-check in step 3: once `close` returns the flag is set, so no new body
dispatch can start; the 0.35 s grace covers the bounded wait plus any body
already in flight.

### Verification plan

1. `cargo build --release` (CI runs roast on release).
2. `time MUTSU_FUDGE=1 prove -e target/release/mutsu roast/S17-supply/syntax.t`
   — **targets: wall < 15 s AND total CPU (`cusr+csys`) < 60 s** (baseline:
   57 s wall / 610 CPU-s; expected result is a few seconds wall with CPU ≈
   wall, since tests 69-71 run in <1 s once test 63 stops leaking).
3. Re-run the repro above: CPU during the trailing `sleep 10` must be ~0 and
   the thread count must fall back to the pool floor (`min(cores,8)` + a few).
4. No-regression sweep (each with `MUTSU_FUDGE=1 prove -e target/release/mutsu`):
   `roast/S17-supply/interval.t`, `roast/S17-supply/on-demand.t`,
   `roast/S17-supply/syntax-nonblocking-await.t`,
   `roast/S17-procasync/basic.t`, `roast/S32-io/IO-Socket-Async.t` (the act
   loop also drives socket/signal taps), plus `prove -e target/debug/mutsu
   t/supply-batch-period.t t/supply-tap-close-interval.t` and a full
   `make test`.
5. Per CLAUDE.md flaky rules: S17 files may need a retry under parallel load,
   but a *concrete subtest* failure (`Failed: N`) in any of the above after
   this change is a real regression, not flakiness. Do not quarantine
   syntax.t in `flaky-tests.txt` — this fix is the reason not to.

### Pitfalls

- **Close ≠ done.** Breaking out of the act loop via the flag must NOT run
  `chain_done_cb` (the whenever's LAST phasers + done-group marker) — raku
  does not fire LAST on `.close`. Exit the loop the way a disconnect does.
  CLOSE phasers are already handled separately by `native_tap` via
  `take_supplier_close_callbacks` (run-once semantics — don't add a second
  firing path).
- **Missed-wakeup risk is bounded, not eliminated** — that is the design: the
  250 ms `recv_timeout` cap means a close that races the worker's wait is
  honoured at most 250 ms late. Never replace it with an unbounded `recv()`
  plus "the closer sends a wake event": the closer has no sender handle, and
  inventing one that lives in the Tap would keep the channel alive.
- **Do not set the closed flag from anywhere except `Tap.close`/`.cancel`.**
  `SupplySender::send`'s new early-return must never trigger for channels
  whose flag is untouched (all other users: Proc::Async, sockets, react
  receivers) — their behavior is unchanged.
- **wasm32**: `worker_pool::submit` delegates to the cooperative scheduler;
  a `recv_timeout` poll loop there would spin the only thread. Keep the plain
  `recv()` under `cfg(target_arch = "wasm32")` (see step 3's cfg fork).
- **Registry hygiene**: the worker must `unregister_act_loop_close` on every
  exit path or the map leaks an entry per tap; `close_act_loop` on an
  already-removed id must stay a no-op (close racing worker exit).
- The GC stance of the act loop's wait (raw block, no
  `block_quiescent`/safepoint) is pre-existing — do not "fix" it in this
  change.
