# ADR-0028: `Supply.schedule-on` genuinely defers tap delivery — callback shims at the tap-registration chokepoint, with a serialized per-tap drain

- Status: Accepted (Slice 1 implemented and Cro-verified 2026-08-13; Slice 2
  bypass-path audit complete 2026-08-13 — two confirmed gaps fixed, one
  deferred to a new deep ticket, two probed and found not to be gaps)
- Date: 2026-08-12
- Related: [ADR-0020](0020-shared-worker-pool.md) (the worker pool the drain
  runs on; supply-lifetime pumps are a sanctioned slice-3 shape there),
  [ADR-0008](0008-push-based-supply-event-delivery.md) (the waker-aware
  `SupplyEvent` channel this design reuses), ADR-0010 (spawn-lineage lexical
  sharing — the drain worker's `clone_for_thread` contract)
- Addresses: `news/2026-08/supply-schedule-on-genuine-tap-deferral.md`
  (Slice 1; the deep ticket resolved into this news entry)

## Context

### The bug (confirmed; full trail in the deep ticket)

`Supply.schedule-on($scheduler)` is documented (`raku-doc/doc/Type/Supply.rakudoc`)
as: *"Runs the emit, done and quit callbacks on the specified scheduler."* In
Rakudo it is implemented as a derived supply that re-dispatches every event
through the scheduler:

```raku
method schedule-on(Supply:D: Scheduler $scheduler) {
    supply {
        whenever self -> \value {
            $scheduler.cue: { emit(value) }
            LAST { $scheduler.cue: { done() } }
            QUIT { my \ex = $_; $scheduler.cue: { quit(ex) } }
        }
    }
}
```

mutsu's implementation (`src/runtime/native_supply_dispatch.rs:457`) only
stashes the scheduler into a `"scheduler"` attribute on a cloned Supply. That
attribute is consulted in exactly one place — the `Supply.interval` wiring in
the `"tap" | "act"` arm of `native_supply_mut`
(`src/runtime/native_supply_mut_methods.rs:144`, `cue_scheduler_interval`).
For every other Supply shape, an upstream emit still reaches the tap callback
**synchronously, on the emitting call stack**.

The consequence is a real deadlock class, confirmed against
`Cro::HTTP::ResponseParser`'s test helper and a Cro-free minimization: a
*blocking* call inside a `.schedule-on($*SCHEDULER)`-wrapped tap callback runs
on the emitting `start {}` thread and prevents that same thread's next
statement (`$fake-in.done()`) — whose effect the blocking call is waiting for
— from ever running. Real Raku deadlocks identically *without*
`.schedule-on()` and passes *with* it, proving the deferral is exactly the
missing semantics, not an incidental fix.

### The chokepoint finding — this is smaller than it first looks

The pre-design scoping pass inventoried the emit side: ~33
`supplier_emit_callbacks(...)` call sites across seven files
(`native_supplier_methods.rs` ×15, `socket_async.rs` ×5,
`supply_transform.rs` ×5, `supply_classify.rs` ×2, plus
`methods_promise.rs`, `methods_supply_dispatch.rs`, `state_supplier.rs`),
each inlining its own `SupplierEmitAction::Call(tap, value, delay)` dispatch,
and concluded there is no shared "run these actions" helper. That is true —
**and it does not matter**, because every one of those sites invokes the
*registered callback `Value`* via `call_sub_value`. What gets registered is
decided in far fewer places, and for a direct `.tap`/`.act` on a Supply it is
decided in exactly **one**: the `"tap" | "act"` arm of `native_supply_mut`
(`src/runtime/native_supply_mut_methods.rs:44`). That arm:

- receives the full attribute map — including the `"scheduler"` key a
  `.schedule-on()` clone carries — *before* any registration path runs;
- funnels the value callback into every registration flavor
  (`register_supplier_tap`, `..._lines_tap`, `..._words_tap`,
  `..._unique_tap`, `..._elems_tap`, `..._with_head_limit`, `..._produce_tap`,
  the on-demand emitter's `register_outer_tap_with_do_callbacks`, and the
  attr-local `"taps"` list used by the `emit` arm);
- also owns the `done =>` / `quit =>` callback wiring
  (`register_supplier_done_callback` / `register_supplier_quit_callback` /
  the immediate-invoke paths).

Wrapping the three callbacks (`tap_cb`, `done_cb`, `quit_cb`) at the top of
that arm therefore covers **all** downstream registration flavors and **all**
~33 emit sites with zero changes to any of them, and composes correctly with
the transformed tap kinds (a lines/words/unique/produce tap invokes
`Call(shim, transformed_value, delay)` — the transformation still runs at the
emit site; only the final delivery defers).

The `whenever`-source registrations that *bypass* the tap arm (the on-demand
body's direct `register_supplier_tap(supplier_id, body_cb, 0.0)` at
`native_supply_mut_methods.rs:414`, the react-loop subscriptions in
`vm/vm_react_subscriptions.rs`, and the internal taps of `.Promise`/`.wait`/
`.list` in `supply_promise.rs`) are deliberately out of Slice 1 — see
"Propagation semantics" and Slice 2 below.

### The primitives already exist

No new concurrency machinery is needed; the design is an assembly of parts
that already carry their own guarantees:

- **`supply_event_channel()`** (`src/runtime/native_methods/supply_channel.rs`)
  — a waker-aware FIFO mpsc of `SupplyEvent::{Emit, Done, Quit}` (ADR-0008).
- **`run_supply_act_loop`** (`src/runtime/native_methods/encoding.rs:444`) —
  the existing pooled drain loop: blocking-recv a `SupplyEvent`, invoke the
  real value/done/quit callback on a `clone_for_thread` interpreter, flush
  output, absorb `done`/`last` control signals, exit on Done/Quit/disconnect.
  It is already the delivery vehicle for channel-backed live supplies
  (`native_supply_mut_methods.rs:236-251`) and socket/whenever readers.
- **`worker_pool::submit`** (`src/runtime/worker_pool.rs`, ADR-0020) — the
  drain worker's home. A supply-lifetime pump borrowing a worker until done is
  an explicitly blessed ADR-0020 slice-3 shape; the elastic starvation check
  means a drain worker blocked inside a user tap callback can never deadlock
  the pool.
- **The synthesized-callable idiom** — `cue_scheduler_interval`
  (`native_supply_mut_methods.rs:1189`) already builds a `SubData` whose body
  is a single `MethodCall` on a literal internal instance
  (`__mutsu_interval_tick`), so schedulers hold an ordinary first-class
  callable. The shims below are the same idiom with a parameter.

## Decision

Implement `.schedule-on()` as **tap-registration-time callback wrapping** at
the `"tap" | "act"` chokepoint, with the deferral vehicle selected by
scheduler kind:

### 1. Scheduler-kind fork (mirrors the existing `.cue` distinction)

At the top of the `"tap" | "act"` arm, when `attrs` contains `"scheduler"`
**and not** `"scheduler_interval"` (the interval wiring keeps its current
path — there the scheduler drives the *ticks*, so delivery is already on it):

- **`CurrentThreadScheduler`** (`class_name == "CurrentThreadScheduler"`, the
  same distinction `native_methods/mod.rs:447-448` encodes as
  `is_current_thread`): **no wrapping, fully synchronous.** Rakudo's
  `CurrentThreadScheduler.cue` runs the block inline, so synchronous delivery
  is the *correct* semantics, not a shortcut. This keeps
  `t/supply-schedule-on.t`, `t/schedule-on-whenever-env.t`, and the
  whitelisted `roast/S17-supply/schedule-on.t` (all of which schedule on a
  `CurrentThreadScheduler`) green with zero behavior change.
- **`ThreadPoolScheduler`** (including `$*SCHEDULER`, which `runtime_init.rs:66`
  creates as one — the Cro case): the **serialized per-tap pump** (§2).
- **Any other Scheduler** (`FakeScheduler`, a user-written class doing the
  `Scheduler` role): **per-event `.cue`** (§3) — its own `cue` method is the
  contract and must not be bypassed with a hardcoded `worker_pool::submit`,
  exactly as `cue_scheduler_interval` already honors for the interval case.

### 2. ThreadPoolScheduler: serialized per-tap pump

At tap time (on the tapping thread, mirroring the existing channel-backed
live-supply path at `native_supply_mut_methods.rs:236-251`):

1. Create a `supply_event_channel()` pair and register the `SupplySender` in
   a new process-global pump registry (`state_supplier.rs` or a sibling;
   `register_scheduled_pump() -> (pump_id, SupplyReceiver)`,
   `scheduled_pump_send(pump_id, SupplyEvent)`,
   `drop_scheduled_pump(pump_id)`), keyed like the existing supply-channel /
   cancellation registries.
2. Build three **shim callables** (the `__mutsu_interval_tick` idiom — an
   empty-env `SubData` whose body is one `MethodCall` on a literal
   `__ScheduledTapPump` instance carrying `pump_id`; empty env and no captures
   make cross-thread invocation trivially safe):
   - emit shim: one parameter, body calls `__mutsu_scheduled_emit($v)` →
     `scheduled_pump_send(id, SupplyEvent::Emit(v))`;
   - done shim: zero parameters → `SupplyEvent::Done`;
   - quit shim: one parameter → `SupplyEvent::Quit(ex)`.
   Dispatch for the `__ScheduledTapPump` class's three native methods is a
   new arm in `native_methods/mod.rs`, next to the other internal classes.
3. Substitute: `tap_cb := emit_shim` (only when the original is an active
   callback), `done_cb := Some(done_shim)` (**unconditionally**, so the drain
   always observes end-of-stream and exits — a `None` user done just means
   the drain's own done slot is empty), `quit_cb := Some(quit_shim)`. Register
   the shims with `delay_seconds = 0.0` — the tap's `:delay` moves into the
   drain loop's `delay_seconds` parameter, which is where a per-event sleep
   belongs (sleeping on the emitting thread is precisely the bug class this
   ADR removes).
4. Submit the drain: `worker_pool::submit(move || run_supply_act_loop(&mut
   interp_clone, &rx, &real_tap_cb, real_delay, real_done_cb, real_quit_cb))`
   where `interp_clone = self.clone_for_thread()` taken at tap time —
   byte-for-byte the existing live-supply pattern.
5. Record `pump_id` on the returned Tap handle; `native_tap`'s
   `"cancel" | "close"` arm (`scheduler.rs:173`) additionally calls
   `drop_scheduled_pump(pump_id)`, whose sender drop disconnects the channel
   and lets the drain exit (the existing subscription close already stops new
   sends).

Then the arm proceeds **unchanged**: every registration flavor registers the
shims; every emit site — all ~33 of them, plus the attr-local `"taps"` path
and the tap-time cold-value replay loop at `native_supply_mut_methods.rs:981`
— invokes a shim, which enqueues and returns immediately.

**Ordering.** Raku delivers a given tap's events in emission order, and the
whitelisted roast pin (`tap-ok ... [1,2,3]`) asserts it. A naive
per-emit `worker_pool::submit` (or a literal per-emit
`ThreadPoolScheduler.cue`, which is the same submit) provides **no ordering
across pool workers** — two queued deliveries can be picked up by two workers
and race. Rakudo gets its ordering from the supply-block serialization lock
its real implementation rides on; mutsu has no equivalent on this path. The
single FIFO channel + single drain worker is the sound, cannot-go-flaky form
of the same guarantee (per the repo's gain/risk definitions), and is why the
"one-liner" fix from the ticket is not the design. The pump also preserves
emit→done ordering for free (Done is just the next event in the queue).

**What this buys the deadlock:** the `start {}` thread's
`$fake-in.emit(...)` drives the supply-block body synchronously (correct —
Rakudo runs supply bodies on the emitting thread too), the body's
`emit $response` reaches the *emit shim*, enqueues, and returns; the start
thread proceeds to `$fake-in.done()`. The drain worker invokes the real tap
callback, whose blocking `.body-text.result` now resolves because the done it
needs was not queued behind it on the same stack. If the tap callback blocks
long-term, the drain worker is a blocked pool worker — ADR-0020's elasticity
absorbs that by design.

### 3. Other schedulers: per-event `.cue`

For a non-native scheduler the shim bodies differ: instead of a channel send,
`__mutsu_scheduled_emit` stores `(real_cb, value)` under a fresh id in a
small registry and calls
`self.call_method_with_values(scheduler, "cue", vec![thunk])` with a
synthesized zero-arg thunk whose body runs the stored pair (same idiom
again). Delivery timing and ordering are then the *scheduler's* contract —
for roast's `FakeScheduler` that means events sit queued until
`progress-by` runs them, which is exactly what a time-controlled test fixture
is for. No pooled drain is created in this flavor. (mutsu already routes
`Supply.interval` ticks through arbitrary schedulers' `.cue` this way —
`t/supply-interval-scheduler.t` pins a user-written scheduler.)

### 4. Propagation semantics (design question 1, resolved against the docs)

The raku-doc contract and Rakudo's implementation agree: scheduling applies
to **delivery to the taps of the schedule-on'd Supply itself** — the derived
supply that `schedule-on` returns cues each of *its own* emit/done/quit
deliveries. A further-derived Supply (`.map`, `.grep`, `.lines` built *on
top* of a scheduled one) is not itself re-scheduled; in Rakudo its operator
body happens to *run* on the scheduler's thread because it taps the scheduled
supply, but there is no per-stage re-cue. mutsu mirrors the observable part:
the `"scheduler"` attribute lives on the schedule-on clone and is not copied
into the fresh attribute maps derived operators build, so only `.tap`/`.act`
on the scheduled Supply (or on a clone that carries the attribute) defers.
Whether mutsu's derived-operator *internal* registrations on a scheduled
source need the same wrapping is observable only through blocking/thread
identity — Slice 2 probes it against `raku` rather than widening blind
(ADR-0027's audit discipline).

## Alternatives considered

1. **Emit-time wrapping: teach every `SupplierEmitAction::Call` site to
   consult the scheduler and defer.** Rejected: ~33 sites across seven files
   today, an unbounded maintenance tax on every future emit path, and it
   still misses the attr-local `"taps"` delivery in the `emit` arm and the
   tap-time cold replay loop. It also has *worse* information available: the
   emit site knows the supplier, not which downstream Supply clone (with
   which scheduler) the subscription came from — that association exists
   naturally only at registration time.
2. **Per-emit `worker_pool::submit` (or literal per-emit
   `ThreadPoolScheduler.cue`) — the ticket's "likely shape".** Rejected on
   ordering: pool workers race, `[1,2,3]` can arrive `[1,3,2]`, and the
   failure would be a load-dependent flake — the exact class the repo's
   risk definition forbids introducing. The serialized pump delivers the same
   deferral with a deterministic order guarantee.
3. **A new `scheduled_sink` field on `SupplierTapSubscription`** (modeled on
   the existing `channel_sink`, which already forwards emits into a channel
   synchronously, interpreter-free, inside the registry lock). Attractive —
   no shim values at all — but rejected: `channel_sink`-style fields
   short-circuit *before* the transformed tap kinds run
   (`state_supplier.rs:903` `continue`s past lines/words/unique/produce
   handling), so `.lines.schedule-on($tp).tap` would silently lose its
   transformation; and done/quit do not flow through the subscription record
   at all (they are separately-registered callback Values), so a second
   mechanism would still be needed for two of the three callback kinds. The
   value-level shim covers all three uniformly and rides the existing
   registration machinery untouched.
4. **Rakudo-verbatim: rewrite `schedule-on` as a real derived
   `supply { whenever self { $scheduler.cue: { emit $_ } } }`.** The purest
   mirror, rejected for now: it stacks the fix on mutsu's heaviest and most
   delicate machinery (the on-demand whenever chain), pays a full supply
   block per scheduled tap, and — decisively — inherits alternative 2's
   ordering hazard, because mutsu has no supply-block serialization lock
   equivalent to the one Rakudo's version implicitly relies on for ordering.
   If mutsu ever grows that lock as a first-class primitive, this becomes the
   natural retirement path for the pump (note it in that campaign's ADR and
   supersede this one).
5. **Do nothing / document as unsupported.** Rejected: this is a confirmed
   real-world deadlock in the Cro::HTTP test suite (the current campaign's
   north star), and the docs' stated purpose of `schedule-on` — decoupling
   delivery from the emitting thread — is exactly what the repro needs.

## Mechanism (implementation plan)

### Slice 1 — the chokepoint wrap for direct `.tap`/`.act` (one PR)

1. Pump registry + `__ScheduledTapPump` native methods
   (`state_supplier.rs` sibling module + a dispatch arm in
   `native_methods/mod.rs`).
2. Shim builders next to `cue_scheduler_interval` in
   `native_supply_mut_methods.rs` (same synthesized-`SubData` idiom).
3. The fork at the top of the `"tap" | "act"` arm: detect
   `"scheduler"` minus `"scheduler_interval"`, classify the scheduler
   (`CurrentThreadScheduler` / `ThreadPoolScheduler` / other), substitute
   `tap_cb`/`done_cb`/`quit_cb` per §1–§3, submit the drain, thread
   `pump_id` onto the Tap handle, extend `native_tap` close.
4. Pins (new `t/supply-schedule-on-defer.t`):
   - the deep ticket's Cro-free repro shape (blocking wait inside a
     `.schedule-on($*SCHEDULER)` tap callback; a sibling `start {}` emit must
     still be observed; `$done` kept within the timeout guard) — **rebuild and
     verify the repro against real `raku` first**; the ticket explicitly warns
     that several plausible simplifications did not reproduce;
   - order preservation: `[1,2,3]` (and a done-after-emits assertion)
     through `.schedule-on(ThreadPoolScheduler.new)`, awaited
     deterministically (a kept Promise in the done callback — no sleeps);
   - quit routing: a `quit =>` handler fires with the right exception through
     the pump;
   - `Tap.close` on a scheduled tap stops delivery and reclaims the drain;
   - CurrentThreadScheduler negative pin: delivery order relative to the
     tapping thread is unchanged (i.e. still synchronous).
5. Existing regression coverage that must stay green untouched:
   `t/supply-schedule-on.t`, `t/schedule-on-whenever-env.t`,
   `t/supply-interval-scheduler.t`, and whitelisted
   `roast/S17-supply/schedule-on.t` (the named canary — it exercises both
   ambient schedulers; run it a few times locally in release before pushing).
   `make test` locally; full roast delegated to CI.

### Slice 2 — audit of the registration paths that bypass the tap arm

Each probed against `raku` with a small repro before changing anything
(ADR-0027's "probe before change, do not widen blind"):

- `whenever $scheduled-supply { ... }` inside another supply block (the
  direct `register_supplier_tap(supplier_id, body_cb, 0.0)` at
  `native_supply_mut_methods.rs:414` reads `supplier_id` off the inner
  attrs and ignores `"scheduler"`);
- `react whenever $scheduled-supply { ... }`
  (`vm/vm_react_subscriptions.rs`) — note the react drive loop is itself a
  delivery boundary, so the observable difference may be nil; establish it
  before touching anything;
- the internal taps of `.Promise` / `.wait` / `.list`
  (`supply_promise.rs`) on a scheduled supply;
- derived operators (`.map`, `.grep`, `.do`, `.lines`, …) applied *after*
  `.schedule-on` — per §4 the final tap does not defer today under mutsu
  (fresh attrs), while Rakudo's runs on the scheduler thread; decide with a
  blocking-shape probe whether that difference is observable in any test
  that matters (the Cro suites are the yardstick).

Each confirmed gap gets its own pin; each non-gap gets a line in the PR
description so the audit is recorded.

### Slice 3 — Cro verification

Re-run `t/http-response-parser.rakutest` from the vendored Cro checkout
(`tmp/cro-work/`, reproduce command in the deep ticket) and record the two
previously-failing subtests ("Response with body terminated by close of
connection", "Connection close with incomplete body throws") in the outcome
note. Not a repo-tracked test (Cro is intentionally not bundled), so this is
verification, not a pin; the Slice 1 Cro-free repro pin is the tracked
stand-in.

## Acceptance criteria

1. The deep ticket's Cro-free repro completes without deadlock: the inner
   promise resolves (`inner status: Kept`) and `done: Kept` prints, matching
   real `raku` with `.schedule-on($*SCHEDULER)`; pinned in
   `t/supply-schedule-on-defer.t`.
2. Emission order and emit-before-done ordering are preserved through a
   `ThreadPoolScheduler` pump, pinned deterministically (promise-based sync,
   no timing assertions — the pin must be flake-proof by construction).
3. `roast/S17-supply/schedule-on.t` stays green (release build, several local
   runs; a deterministic failure is a design bug per the triage protocol).
4. `t/supply-schedule-on.t`, `t/schedule-on-whenever-env.t`, and
   `t/supply-interval-scheduler.t` pass unchanged — CurrentThreadScheduler
   stays synchronous, interval-on-a-scheduler wiring keeps precedence, and a
   user-written scheduler still receives `.cue` calls rather than being
   bypassed.
5. A closed Tap on a scheduled supply stops delivery and releases its drain
   worker (no pump leak per tap churn — the shape Cro's per-connection
   pipelines exercise).
6. No `make test` regressions locally; full `make roast` delegated to CI
   (this touches Supply tap dispatch — a local subset is not sufficient).
   `S17-*` failures on CI get the flaky-triage protocol, not a shrug.
7. On completion, `git mv` the deep ticket to a `news/2026-08/` entry and
   rewrite it as an accomplishment, per `todo/README.md`.

## Outcome (Slice 1, 2026-08-13)

Implemented as designed, with two adjustments discovered only by running the
mechanism against real dispatch (both one-line-scope fixes, not design
changes):

- **The dispatch gate is two-layered.** `call_native_instance_method[_mut]`'s
  `class_name` allowlist in `native_methods/mod.rs` is necessary but not
  sufficient — the generic method-call resolver first checks
  `Interpreter::is_native_method(class_name, method)` against a per-class
  `ClassDef.native_methods` set (`runtime_init.rs`) before it ever reaches
  that dispatcher; an unlisted method there fails with "No such method" even
  though the native arm itself was wired correctly. `__ScheduledTapPump`
  needed its own `ClassDef` entry (mirroring `Tap`'s) listing its four
  methods, the same way `Supplier` lists `__mutsu_interval_tick`.
- **A scheduler used as a bare, undefined type object (`CurrentThreadScheduler`
  written directly rather than `.new`-ed) is a `ValueView::Package`, not a
  `ValueView::Instance`.** Real Raku dispatches `.cue` on it as a class
  method regardless of definedness; the scheduler-kind classification had to
  read `class_name` off both `ValueView::Instance` and `ValueView::Package`,
  or an undefined `CurrentThreadScheduler` silently fell into the §3
  any-other-scheduler fork and failed with "No such method 'cue'" the first
  time delivery actually needed to run.

Every acceptance criterion above was verified against real `raku` before
being pinned (`t/supply-schedule-on-defer.t`, 5 cases, all cross-checked
against `raku` output first per the deep ticket's own warning that
plausible-looking simplifications don't always reproduce). `make test`
(3095 files / 28783 tests) and the full `roast/S17-supply` +
`S17-lowlevel`/`S17-promise`/`S17-procasync` whitelists are green on a
release build, several runs each.

**Slice 3 (Cro verification) done as part of this same change**, ahead of
schedule: re-running `t/http-response-parser.rakutest` from the vendored Cro
checkout went from a mid-file hang/timeout to 155/156 completing — both of
the deep ticket's originally-reported failures ("Response with body
terminated by close of connection", "Connection close with incomplete body
throws") are now `ok`. The single remaining `not ok` (a `.body-text` call
under `Content-length: 1000` with a connection close after far fewer bytes,
expected to throw `X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`) is
a different, narrower bug filed separately:
`todo/tickets/http-response-parser-content-length-too-short-not-thrown.md`.
Cro::HTTP suite: 34/35 fully-green (up from 33/35), remaining gap being that
file plus the unrelated pre-existing `http2-request-parser.rakutest`
concurrent-stream ticket. Cro::Core stays 9/9.

**Update (2026-08-13, later the same day):** that ticket is resolved —
three compounding general bugs (a `whenever` `LAST`-phaser die not
converting to quit; a nested-derived-source quit leaving a
`Promise(supply {...})` coercion `Planned` forever; `.cause`/`.result`
re-wrapping a user exception in `X::AdHoc` via a name-based ancestry check).
See `news/2026-08/whenever-last-phaser-die-and-broken-promise-exception-type.md`.
`http-response-parser.rakutest` is now 156/156; Cro::HTTP suite 34/35
fully-green (sole remaining gap: the unrelated `http2-request-parser.rakutest`
concurrent-stream ticket).

**Slice 2 (bypass-path audit) is done (2026-08-13).** Each of the four
candidates in the Slice-2 section above was probed against real `raku` with a
thread-identity probe and, where that showed a difference, a deadlock-shape
probe mirroring the Slice-1 repro (mutsu deadlocks — `Planned` — exactly where
raku resolves — `Kept` — confirms a *real* gap, not just a cosmetic thread-id
difference; several probes needed a same-thread-race fix, or hit unrelated
raku quirks, before they were trustworthy — see below):

- **Confirmed gap, fixed:** `whenever $scheduled-supply { ... }` inside
  another `supply { }` block. Its direct `register_supplier_tap(supplier_id,
  body_cb, 0.0)` call (bypassing the chokepoint) now routes through a new
  shared helper, `wrap_scheduled_callbacks` (extracted from the `"tap"|"act"`
  arm's inline scheduler-classification code, zero behavior change there —
  verified against the existing Slice-1 pins before adding anything new),
  keyed off the *inner* (whenever-source) Supply's own attrs rather than the
  outer supply block's. A `ThreadPoolScheduler` pump created this way is
  threaded onto the upstream-close cascade as a nested synthetic `Tap`
  instance (reusing `close_upstream_taps`'s existing Instance-recursion
  branch) so `Tap.close` on the *outer* supply block still reclaims the
  nested pump — no separate cleanup mechanism needed.
- **Confirmed gap, fixed:** deferred-registration derived operators
  (`.lines`, `.words`, `.unique`, `.elems`, `.produce` — `.head` already
  worked, since its live branch `attributes.clone()`s the whole map) applied
  *after* `.schedule-on()`. These build a fresh attrs map carrying
  `supplier_id` forward and defer actual tap registration to whenever the
  caller eventually calls `.tap()`/`.act()` on the derived Supply — which
  *does* hit the chokepoint, so the only gap was that none of them copied
  `"scheduler"` into that fresh map. Fixed by copying it forward alongside
  the existing `supplier_id` copy at each of the 5 sites (mechanical,
  low-risk).
- **Confirmed gap, NOT fixed — new deep ticket:** `.map`/`.grep`/`.do` (all
  three share `make_live_transform_supply`) and `.flat` register their
  transform tap *immediately* at call time, directly on the source's raw
  `supplier_id` via `register_supplier_transform_tap`/`register_supplier_flat_tap`
  — a `TransformState` consulted synchronously at the emit sites, which never
  sees the "scheduler" attribute (it isn't visible from there; the Supply
  *value* that carries it isn't in scope at that layer). This is
  architecturally different from the two fixes above — no attrs map to
  thread `"scheduler"` through, and no existing tap-callback registration
  point to route through `wrap_scheduled_callbacks`. A correct fix needs a
  new synthesized-shim class following the `__ScheduledTapPump` idiom;
  design sketch, confirmed repro, and affected-file inventory are in
  `todo/deep/schedule-on-live-transform-operators-bypass-deferral.md`.
- **Probed, NOT a gap:** `react whenever $scheduled-supply { ... }`. A naive
  thread-identity probe was misleading here — mutsu's react drive loop
  already runs on its own dedicated driving thread even *without*
  `.schedule-on()` (architecturally different from Rakudo, where an
  unscheduled `whenever` inside `react` runs synchronously on the emitting
  thread), so "different thread" doesn't distinguish scheduled from
  unscheduled in mutsu's react. A deadlock-shape probe (mirroring the other
  two confirmed gaps, with a blocking `await` + `done` phaser inside the
  whenever body) was inconclusive in a different way: it hit a **pre-existing
  raku quirk unrelated to scheduling** — the same non-`Kept` outcome
  reproduced in real raku *with and without* `.schedule-on()`, isolating it
  as an await/react/`done`-phaser interaction quirk, not a scheduling gap.
  No fix, no pin — recorded here per the ADR's "each non-gap gets a line"
  rule.
- **Probed, NOT a gap:** the internal taps of `.Promise`/`.list`. The first
  attempt at a `.Promise` probe showed a false gap (`Planned` in mutsu vs
  `Kept` in raku) that turned out to be a **test race**, not a real one: the
  probe spawned the emitting `start {}` concurrently with the `.Promise`
  coercion's own tap registration, so the emit could beat the tap into
  existence. Removing the race (register the Promise coercion on the main
  thread *before* spawning the emitter) showed `Kept` for both scheduled and
  unscheduled sources in mutsu, matching raku — no gap. `.list` on a *live*
  (Supplier-backed, cross-thread) source returned an empty list in mutsu
  **even without `.schedule-on()`** — a real bug, but unrelated to this ADR
  (out of scope for Slice 2; not filed as a new ticket in this pass, since
  `Supply.list` on a static/eager source works fine per the existing
  `t/supply-list.t` coverage and this narrower live-cross-thread shape wasn't
  independently confirmed beyond the one probe).

Pinned in `t/supply-schedule-on-defer-nested-whenever.t` (3 cases, all
cross-checked against real `raku` first): the `whenever`-in-supply deadlock
repro, the `.lines`-after-`schedule-on` deadlock repro, and a `Tap.close`
cascade-reclaims-the-nested-pump case. `make test` and the whitelisted
`roast/S17-supply` (release) stay green; the Slice-1 pins
(`t/supply-schedule-on-defer.t`, `t/supply-schedule-on.t`,
`t/schedule-on-whenever-env.t`, `t/supply-interval-scheduler.t`) are
unaffected by the `wrap_scheduled_callbacks` extraction (verified before and
after — identical pass/fail).

## Risks

- **Drain-worker lifetime.** A scheduled tap on a supply that never
  completes parks a pooled worker on a blocking `recv` for the process
  lifetime — the same cost profile as the existing channel-backed act loops
  and ADR-0020's supply-lifetime pumps, and strictly cheaper than the
  pre-pool dedicated threads. Tap close reclaims it (criterion 5).
- **Cross-thread tap callbacks.** Tap callbacks that previously ran on the
  emitting (often main) thread now run on a pool worker under
  `clone_for_thread` — the ADR-0010 lineage store and the closure-cell
  campaigns (ADR-0023/0025/0027) are what make captured lexicals behave; this
  is the most heavily exercised shape of the whole Cro campaign, but a
  latent gap would surface here as a new deterministic failure, which is the
  safety net working (CI carries the full roast).
- **Timing-shape changes.** Any test that taps through a
  `ThreadPoolScheduler`-scheduled supply and assumes synchronous delivery
  changes behavior — by design (Raku defers there too). The inventory found
  no such test in `t/` or the whitelist (all existing coverage schedules on
  `CurrentThreadScheduler`); the Cro suites want the new behavior.
