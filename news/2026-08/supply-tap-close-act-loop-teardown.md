# Tap.close now tears down channel-backed act-loop workers (S17-supply/syntax.t: 610 → 7 CPU-seconds)

`roast/S17-supply/syntax.t` used to take ~57 s wall / ~610 CPU-seconds on a
release build (raku: well under a second), and its gc-stress configuration was
a deterministic timeout (exit 124 at 70/90 after burning 2081 CPU-s at ~950 %
CPU). It was the top entry in the CI flake survey and killed jit-stress and
gc-stress jobs on runner-load luck.

## Root cause

`Tap.close` never stopped the pool worker driving a channel-backed supply
source. Tapping a `supply { whenever Supply.interval(...) { ... } }` block (or
`Supply.interval(...).tap` directly) submits `run_supply_act_loop` to the
worker pool; the worker owns the channel's `SupplyReceiver` and blocks in
`recv()`, while the interval-timer heap holds the `SupplySender` and keeps
sending a tick every period until a send fails. Nothing recorded on the Tap
handle could reach either half, so after `.close` the timer kept ticking and
the worker kept dispatching the `whenever` body through the full interpreter
path — forever, until process exit.

Test 63 of syntax.t closes 4000 such taps; the leaked workers (~3400 live
threads at ~950 % CPU) then ran for the rest of the file, which is why tests
69–71 appeared to take 12–30 s each (they run in <1 s in isolation). The same
leak burned the gc-stress budget: each leaked body dispatch is allocation
churn that GC_VERIFY re-verifies.

## Fix

A close-flag protocol, no condvar hand-off needed because the wait is bounded:

- `supply_event_channel()` now creates a shared `closed: Arc<AtomicBool>`;
  `SupplySender::send` fails once it is set (the interval timer already
  treats a failed send as "receiver gone" and retires its heap entry), and
  `SupplyReceiver` exposes `recv_timeout` and a `close_flag()` handle.
- A registry (`register_act_loop_close` / `close_act_loop` /
  `unregister_act_loop_close` in `native_methods/state_lock.rs`) maps fresh
  ids to close flags; the two leaking tap sites in
  `native_supply_mut_methods.rs` register each spawned act loop and record
  the ids on the Tap handle as `act_loop_close_ids`.
- `run_supply_act_loop` takes the flag: its blocking `recv()` becomes a
  bounded 250 ms `recv_timeout` loop that re-checks the flag (and re-checks
  after a successful receive, so no body dispatch starts after `.close`
  returns — the pin test relies on that). Flag-driven exit is a *close*, not
  a `done`: the done chain (LAST phasers) does not run, matching raku. The
  scheduled-pump drain keeps the plain blocking receive (its sender is
  dropped on close), as does wasm32 (a timeout poll would spin the
  cooperative scheduler's only thread).
- `native_tap` "close"/"cancel" walks `act_loop_close_ids` and sets the flags.

## Measured results (release build)

- `syntax.t` plain: 57 s wall / 610 CPU-s → **3 s wall / 6.9 CPU-s**.
- `syntax.t` gc-stress env (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024
  MUTSU_GC_VERIFY=1`, scale 2): deterministic timeout at 240 s (2081 CPU-s)
  → **PASS in 4 s wall / 7.7 CPU-s**. This also resolves
  `todo/deep/s17-supply-syntax-gc-stress-budget.md` — the "wall-clock
  regression" was this leak, not GC_VERIFY cost.
- Leak repro (4000 tap+close, then `sleep 10`): was 3379 live threads at
  956 % CPU during the sleep (+36 CPU-s burned while "idle"); now the whole
  run is 5.7 CPU-s total with the trailing sleep at ~0 % CPU. CLOSE phasers
  still fire exactly once per tap.

Pin: `t/supply-tap-close-interval.t` (deterministic — after `.close` returns,
no new tick can be dispatched; the 0.35 s grace covers the bounded wait plus
an in-flight body).
