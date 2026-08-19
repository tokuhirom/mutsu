# ADR-0031: A supply block's quit belongs to its own emitter, and a cold `whenever` source is tapped rather than replayed

- Status: Partially implemented — Slice 1 (Decision A, quit ownership) shipped
  2026-08-19; Slice 2 (Decision B, `supply_get_values` tap-and-drain) and
  Slice 3 (retire the ticket) are not started. See "Outcome" below.
- Date: 2026-08-19
- Related: [ADR-0008](0008-push-based-supply-event-delivery.md) (the sink/waker
  primitives the drain in Decision B rides on), [ADR-0028](0028-supply-schedule-on-deferred-tap-delivery.md)
  (the `"tap" | "act"` chokepoint and its four whenever-source branches, which
  this ADR reuses rather than adds to), [ADR-0020](0020-shared-worker-pool.md)
  (the pool a supply-lifetime drain borrows from)
- Addresses: `todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`

## Context

### The deep ticket, re-measured on 2026-08-19 — half of it has evaporated

The ticket (filed 2026-07-25 from `Test::Scheduler`, `TODO_dist` T-037) reported
two defects behind one repro:

1. `supply_get_values` recognises a `whenever` subscription marker only when its
   source is a `Supply`, so a `Promise`-sourced marker leaked to the tap as a raw
   4-element array;
2. a cold (supplier-less, on-demand) supply used as a `whenever` source is
   *replayed synchronously* rather than tapped, so emissions that arrive later
   than the replay are invisible.

Re-running the ticket's own repro against `530ccf7dd` shows **defect 1 no longer
reproduces at all**. The recorded symptom was "six 4-element marker arrays";
today mutsu prints:

```
raku:   ["badger", "badger"]   died=True
mutsu:  ["badger", "badger", "badger"]   died=False
```

No markers leak. `normalize_promise_whenever_markers` plus the on-demand
chained-tap branch (`native_supply_mut_methods.rs:739`, "chain a REAL tap so
liveness propagates") landed between the filing and now, and between them they
fixed the *value* half of the ticket on the `.tap`/`.act` path. `.list` on the
same shape is likewise correct today (`("badger", "badger")`, matching raku).

What is left is **not a value-delivery bug at all — it is a quit-propagation
bug**, plus a genuinely-still-real, narrower residue of defect 2. The ticket's
"Root cause" section is therefore stale and must not be used to plan the work;
this ADR replaces it.

### Defect A (the repro's actual cause): quit ownership is attached to the wrong object

In Raku, `die` inside a `whenever` body quits **the enclosing `supply` block**,
and that quit is delivered to whoever tapped *that* supply. Verified against
`raku` (probe `tmp/probe6.raku`): a body `die` does **not** run that same
`whenever`'s own `QUIT` phaser — it goes straight to the tap's `quit =>`
handler. A quit coming *from the source* does run the `QUIT` phaser first, and
only reaches the tap if the phaser leaves it unhandled. mutsu already matches
this for one source shape and diverges for the rest.

The reason is that mutsu attaches the downstream `quit =>` handler to whichever
*upstream* object each of the four whenever-source branches in the
`"tap" | "act"` arm happens to have in hand:

| branch (`src/runtime/native_supply_mut_methods.rs`) | where the tap's `quit =>` is registered |
|---|---|
| b1 — supplier-backed source (`:487`) | `register_supplier_quit_callback(<inner source supplier_id>)` at `:575` |
| b2 — channel-backed source (`:646`) | `register_supplier_quit_callback(<emitter_supplier_id>)` at `:679` |
| b3 — chained on-demand source (`:739`) | **nowhere** |
| b4 — cold replay (`:837`) | invoked inline from `replay_cold_whenever_capture`'s returned `unhandled_quit` |
| nested `whenever <Promise>` (`src/runtime/subtest.rs:596`) | body error is **discarded** (`let ran = …; if ran.is_ok()`) |

Only b1 and b2 work, and only by accident: the `die` unwinds through
`supplier_emit_callbacks` dispatch on the very supplier the handler happens to
be attached to, where `native_supplier_methods.rs:140-150` routes a failed tap
callback to `take_supplier_quit_callbacks(<that supplier>)`.

Measured consequences (all reproduced on `530ccf7dd`, all correct under `raku`):

- **b3 gap** (`tmp/probe3.raku` case C): `supply { whenever <cold on-demand
  supply> -> $v { emit $v; die "boom" } }`, tapped with `quit => {…}` — raku:
  `died=True`; mutsu: `died=False`, the exception vanishes. Adding a second,
  supplier-backed `whenever` to the same block (`tmp/probe7.raku`) does not help,
  confirming the handler is simply not reachable from the failing dispatch.
- **Nested-promise gap** (`tmp/probe3.raku` case B): `whenever Promise.in(…) {
  die … }` registered from inside another `whenever`'s body — raku: `died=True`;
  mutsu: `died=False`. `run_whenever_with_value`'s `ValueView::Promise` arm runs
  the body on a thread clone inside `on_resolve` and throws the `Result` away.
- Together these are exactly the ticket's repro: its `die "Timed out"` sits in a
  nested `whenever Promise.in($timeout)` (nested gap) inside a `whenever` whose
  source is a cold on-demand supply (b3 gap). Both must be closed for the repro
  to reach `died=True`, and once it does, the third `'badger'` also disappears,
  because a quit terminates the supply.

### Defect B (the ticket's claim 2, narrowed): only `supply_get_values` still replays

The `.tap`/`.act` path (b3) and the `.Promise`/react path
(`register_nested_on_demand_source`, `src/vm/vm_react_supply_helpers.rs:63`)
both chain a *real* tap for a cold on-demand source. The synchronous replay
survives in exactly one family: `supply_get_values`
(`src/runtime/supply_promise.rs:239`) and its two replay helpers
`replay_cold_whenever_capture` (`:676`) and `replay_static_whenever_promise`
(`:789`). That family feeds ~20 call sites — `.list` / `.List` / `.Seq` /
`.wait` / `.Channel` / `.sort` / `.squish` / `.head` / `.flat` / `.produce` /
`.batch` / `.rotor` / `.rotate` / `.comb` / `.snip` / `.minmax` / `.zip` /
`.start` / `.throttle` / `.stable`.

Replay works only when every emission happens *during the synchronous call*. It
gets promise-sourced inner subscriptions right by blocking on `shared.wait()`
(`supply_promise.rs:271`), and it silently **drops** any live inner subscription
(`if is_live { continue; }`, `:328-330`). Measured (`tmp/probe5.raku` case E):

```raku
my $supE = Supplier.new;
my $srcE = supply { whenever $supE.Supply -> $v { emit $v } }
my $outE = supply { whenever $srcE   -> $v { emit $v } }
start { sleep 0.05; $supE.emit('e1'); $supE.emit('e2'); $supE.done }
say $outE.list;     # raku: (e1 e2)    mutsu: ()
```

A related symptom on the same family's sibling path: `await` on a supply whose
`whenever` source is a cold on-demand supply returns `Nil` where raku returns
the last emitted value (`tmp/probe4.raku`) — `supply_promise_on_demand` still
finishes through `replay_static_whenever_promise` in that shape.

### The unifying architectural statement

mutsu has **three parallel implementations of "deliver a `whenever` source"**:
the tap chain (`"tap" | "act"`), the react drive loop
(`drive_react_subscriptions_*`), and synchronous replay (`supply_get_values`).
The first two are push-based and handle every source flavour; the third is
pull-based and can only see the present instant. Replay is the odd one out, and
every remaining bug in this area is a consequence of it or of the ad-hoc
per-branch bookkeeping that grew around it. This ADR takes two steps toward
collapsing three into two.

## Decision

### A. The enclosing supply block's **emitter supplier** owns quit

A `supply` block's emitter supplier (`emitter_supplier_id`, minted at
`native_supply_mut_methods.rs:367`) already stands for "this supply" for
`emit` (`register_outer_tap_with_do_callbacks`), for `done`
(`make_on_demand_complete_marker`) and for CLOSE
(`make_supply_close_marker`). Make it stand for `quit` too:

1. **Register the tap's `quit =>` handler once, on `emitter_supplier_id`**, in
   the on-demand branch before the marker walk — instead of per-source inside
   b1/b2 and not at all in b3. Concretely: hoist the `register_supplier_quit_callback`
   currently at `:679` (b2) out of the branch, delete the one at `:575` (b1),
   and drop nothing in b3/b4 — they are then covered by the hoisted one.
2. **Teach `call_supply_tap` the dual of its existing `done` absorption.**
   `call_supply_tap` (`src/runtime/supply_promise.rs:20`) already knows whether
   the callback it is about to run is a *stamped* `whenever` callback (it reads
   `WHENEVER_EMITTER_ENV_KEY` to get the callback's own emitter, and already
   converts a bare `done` raised in the body into
   `self.call_method_with_values(e, "done", vec![])`, `:88-90`). Add the
   symmetric arm: when the call returns an `Err` that is **not** a control
   signal (`is_return` / `return_value` / `is_react_done` / `is_last` /
   `is_supply_body_done` / `is_next` / `is_redo`) and the callback is stamped
   with an emitter that carries a `supplier_id`, convert it to
   `self.call_method_with_values(e, "quit", vec![reason])` and return
   `Ok(Value::NIL)`.

   `Supplier."quit"` (`native_supplier_methods.rs:488`) is already the canonical
   quit routine: it sets the terminal state (so later emits are ignored via
   `supply_is_terminated`), runs `take_supplier_whenever_quit_callbacks` first
   with the `QuitOutcome::{Handled, HandledViaDone, Unhandled}` protocol, then
   the downstream `quit_callbacks`. Reusing it is what keeps the source-quit
   semantics (probe G) intact while adding the body-die semantics (probe F).
3. **Route `run_whenever_with_value`'s `ValueView::Promise` arm
   (`src/runtime/subtest.rs:596-611`) through `call_supply_tap`** instead of the
   raw `call_sub_value`, so a nested `whenever <Promise>` body's `die` reaches
   the same conversion. The callback is already stamped there (the `stamp`
   closure at `:386` runs for every callback this function builds), so nothing
   else changes.

**Why stamped-vs-unstamped is the right discriminator.** A plain `.tap({ … })`
callback carries no emitter stamp, so `call_supply_tap` leaves its `Err`
untouched and the existing fall-through at `native_supplier_methods.rs:140-150`
keeps handling it exactly as today. Only callbacks that are literally a
`whenever` body — the ones for which "quit the enclosing supply" is the spec —
take the new arm. When the stamped emitter has no `supplier_id` (the
`run_on_demand_body(cb, None)` shape used by the replay path), there is nothing
to quit: fall through and return the `Err` as today.

**What this fixes, and why the ticket's repro then matches raku.** The nested
`whenever Promise.in($timeout)` body's `die "Timed out"` becomes
`$timed-out-emitter.quit(X::AdHoc("Timed out"))`; the hoisted registration means
the tap's `quit => { $died = True }` is on that very emitter; and the terminal
state stops the third `'badger'` from being delivered. `["badger", "badger"]` /
`died=True`.

### B. `supply_get_values` taps and drains instead of replaying

Replace the replay worklist in `supply_get_values` with a **collecting tap plus
a bounded drain**, reusing the `"tap" | "act"` arm that already handles all four
source flavours:

1. New helper, `Interpreter::supply_collect_values(&mut self, supply: &Value,
   wait_until_done: bool) -> Result<Vec<Value>, RuntimeError>`, next to
   `supply_get_values` in `src/runtime/supply_promise.rs`:
   - mint a `SharedPromise` as the done signal and a collector sink (a Rust-side
     `Arc<Mutex<Vec<Value>>>` reachable from a synthesized callable built with
     the `__mutsu_interval_tick` / `__ScheduledTapPump` idiom that ADR-0028 §2
     already established — an empty-env `SubData` whose body is one `MethodCall`
     on a literal internal instance, so it is trivially safe to invoke
     cross-thread);
   - `self.call_method_with_values(supply.clone(), "tap", vec![collector,
     done => keep-the-promise, quit => break-the-promise])`;
   - block on the promise through the ADR-0008 waker primitives with a bounded
     deadline (the same 30s budget `supply_promise_on_demand` uses at
     `supply_promise.rs:621`), then close the tap and return the collected
     values.
2. `supply_get_values(&AttrMap)` keeps its signature for its ~20 callers and
   becomes a thin wrapper: rebuild the Supply value (`Value::make_instance(
   Symbol::intern("Supply"), attrs)`) and delegate.
3. `replay_cold_whenever_capture` and `replay_static_whenever_promise` are
   deleted once their two and one remaining callers are migrated.

**Guarding against a new hang.** Today's replay always returns; a drain can
block. Two rules keep the change sound rather than merely correct-in-theory:

- **Do not wait when nothing can arrive.** `run_on_demand_body` already reports
  `body_ran_done`; when the body completed synchronously and registered no live
  subscription, return the collected values immediately without touching the
  promise — the current fast path, preserved.
- **The deadline is a bug-detector, not a semantic.** A drain that hits the
  deadline is a mutsu defect, not a user program's; it must be observable
  (return what arrived, as replay does today) rather than silently eternal.
  An infinite supply passed to `.sort` hangs in raku too, so matching raku there
  is not a regression to engineer around.

## Alternatives considered

1. **Implement the ticket as written** — make `supply_get_values`'s marker
   expansion skip a `Promise`-sourced marker the way it skips a live one.
   Rejected: the symptom no longer reproduces (measured above), and the ticket
   itself notes the change "would then deliver nothing at all". Acting on the
   stale analysis would have produced a regression dressed as a fix.
2. **Register the tap's `quit =>` on every upstream source supplier.** The
   minimal patch for the b3 gap. Rejected: it fires N times for N sources,
   and it is semantically wrong — an upstream quit that the whenever's own
   `QUIT` phaser *handles* (probe G, `QuitOutcome::Handled`) must not reach the
   tap, and a per-source registration cannot distinguish that from a body die.
   The emitter is the only object that means "this supply".
3. **Convert the body die to a quit at each of the ~33
   `SupplierEmitAction::Call` sites.** Rejected for the reason ADR-0028 §
   "Alternatives" already established for the emit side: those sites know the
   supplier, not which supply block the callback belongs to. The stamp on the
   callback carries exactly that association, and `call_supply_tap` is the one
   place every one of those sites already funnels through.
4. **Keep replay, but make it recursively tap live inner sources.** Rejected:
   that is tap-and-drain with extra steps, and it still cannot observe a value
   that arrives after the synchronous call returns — the defining limitation.
5. **Rakudo-verbatim: give supply blocks a real serialization lock and rebuild
   all three delivery paths on it.** The eventual right answer and the natural
   retirement path for both this ADR's Decision B and ADR-0028's pump, but far
   beyond one campaign; noted here so a future lock ADR knows to supersede this
   one.

## Mechanism (implementation plan)

### Slice 1 — quit ownership (Decision A)

Small, self-contained, and it is what closes the ticket's repro.

1. `src/runtime/supply_promise.rs`, `call_supply_tap`: extend the terminal
   `match (res, stamped, emitter)` with the non-control-`Err` → `$emitter.quit`
   arm described in A.2, gated on the emitter carrying a `supplier_id`.
2. `src/runtime/subtest.rs:596-611`: `run_whenever_with_value`'s Promise arm
   calls `call_supply_tap` for the body (and for the LAST/QUIT callbacks, for
   the same emitter-stamp reason).
3. `src/runtime/native_supply_mut_methods.rs`: hoist the tap's `quit =>`
   registration to `emitter_supplier_id` once, before the marker walk; remove
   the b1 (`:575`) and b2 (`:679`) per-source registrations. Leave b1's
   `register_supplier_whenever_quit_callback` (`:598-605`) alone — the whenever's
   own `QUIT` phasers stay bound to their source, which is what probe G pins.
4. Pins, all cross-checked against `raku` first (a new `t/supply-whenever-body-die-quits-block.t`):
   - the deep ticket's `Test::Scheduler` repro verbatim → `["badger","badger"]`,
     `died=True`;
   - probe3 case B (`die` in a nested `whenever <Promise>` body);
   - probe3 case C (`die` in a `whenever` body whose source is a cold on-demand
     supply);
   - probe6 case F (a body `die` does **not** run that whenever's own `QUIT`
     phaser) and case G (a *source* quit does, and a handled one suppresses the
     tap's `quit =>`) — the negative pins that keep A from over-firing;
   - a plain `.tap({ die … })` on a live Supply still routes through the
     existing unstamped path.
5. Regression watch: the whitelisted `roast/S17-supply/*` (release, several
   runs) plus `t/supply-*.t`; `make test` locally, full roast to CI. Quit
   routing is load-bearing for the Cro pipelines, so the ADR-0028 pins
   (`t/supply-schedule-on-defer*.t`) are named canaries.

### Slice 2 — `supply_get_values` taps and drains (Decision B)

1. Land `supply_collect_values` alongside the existing replay and switch
   **`.list` / `.List` / `.Seq` / `.wait` only** (the `supply_list_values`
   feeder at `src/runtime/methods_call_helpers.rs:52`), with probe5 case E as
   the pin. Measure `roast/S17-supply` before and after.
2. Switch the `native_supply_dispatch.rs` combinators (`.sort`, `.squish`,
   `.head`, `.flat`, `.produce`, `.batch`, `.rotor`, `.rotate`, `.comb`,
   `.snip`, `.minmax`, `.zip`, `.start`, `.Channel`) plus
   `supply_transform.rs`'s `.throttle` / `.stable`.
3. Switch `supply_promise_on_demand`'s `replay_static_whenever_promise` call
   (`supply_promise.rs:563`); probe4's `await`-returns-`Nil` is the pin.
4. Delete `replay_cold_whenever_capture` and `replay_static_whenever_promise`.

Each step is independently shippable and independently revertible; step 1 is
the one that carries the measured user-visible gap.

### Slice 3 — retire the ticket

`git mv todo/deep/cold-supply-whenever-source-replayed-not-tapped.md
news/YYYY-MM/…` and rewrite as an accomplishment, per `todo/README.md`. Re-check
`Test::Scheduler`'s `t/synopsis.rakutest` and `t/virtualized-time.rakutest`
(`TODO_dist` T-037) and record the new counts.

## Acceptance criteria

1. The deep ticket's repro prints `["badger", "badger"]` / `died=True`, matching
   `raku`, and is pinned.
2. probe3 B and C, probe6 F and G all match `raku` and are pinned; the F/G pair
   proves the change did not turn a body die into a `QUIT`-phaser event or a
   handled source quit into a tap quit.
3. probe5 case E (`.list` through a cold source with a live inner subscription)
   returns `("e1", "e2")` — Slice 2 step 1.
4. probe4 (`await` on a supply with a cold on-demand `whenever` source) returns
   the last emitted value instead of `Nil` — Slice 2 step 3.
5. `roast/S17-supply` whitelist stays green on a release build across several
   runs; a deterministic failure there is a design bug, per the triage protocol,
   not a flake.
6. No new drain can hang a previously-terminating program: every `.list` /
   combinator in `t/` and the S17 whitelist completes within its existing time
   budget.

## Risks

- **Over-firing the new quit arm.** If the control-signal exclusion list in
  `call_supply_tap` is incomplete, an ordinary `next`/`last`/`return` from a
  whenever body would tear the supply down. The list is already enumerated at
  `native_supplier_methods.rs:108-136` for the same decision; reuse it verbatim
  rather than re-deriving, and pin each signal.
- **Double delivery of a quit.** The emit-dispatch fall-through
  (`native_supplier_methods.rs:140-150`) and the new arm must not both run.
  Returning `Ok(Value::NIL)` after the conversion is what prevents it; a pin
  that counts `quit =>` invocations (must be exactly 1) is cheap insurance.
- **Slice 2 turning a fast path into a blocking one.** The `body_ran_done`
  fast path and the bounded deadline are the guards; the concrete danger shape
  is a producer that runs on the *calling* thread (the deadlock class ADR-0028
  documents), so Slice 2 step 1 must probe that shape explicitly before the
  combinators follow.
- **Cro is the yardstick, and it is downstream of both.** Quit routing feeds
  `Cro::HTTP`'s error paths and `.list`/`.wait` feed its body coercions. Run the
  vendored Cro suites (`modules/`, `scripts/battery-testsuite.sh`) as part of
  Slice 1 and Slice 2 step 1, not only at the end.

## Outcome

### Slice 1 (Decision A), shipped 2026-08-19

Implemented as designed, with one addition the design did not anticipate:

1. `call_supply_tap` (`src/runtime/supply_promise.rs`) gained the non-control
   `Err` → `$emitter.quit($reason)` arm, gated on the callback being stamped
   and its emitter carrying a `supplier_id` — using the control-signal
   exclusion list from `native_supplier_methods.rs` verbatim, as the ADR
   specified.
2. `run_whenever_with_value`'s `ValueView::Promise` arm (`src/runtime/subtest.rs`)
   now calls the body through `call_supply_tap` instead of raw
   `call_sub_value`. Preventing the whenever's own LAST phaser from *also*
   firing after a converted die (both `ran.is_ok()` after the conversion)
   needed an explicit post-call termination check — `emitter_supplier_id_of`
   plus `supplier_snapshot(sid).2.is_some()` — that the ADR's text did not
   spell out.
3. `native_supply_mut_methods.rs`'s on-demand `"tap"|"act"` branch now
   registers the tap's `quit =>` handler once on `emitter_supplier_id`,
   before the marker walk; the old per-source registrations in b1 and b2 are
   removed, exactly as designed.

**Gap the ADR did not anticipate, found by testing probe6 case G (a source's
own `.quit()` call, not a body die) and fixed in the same PR:** removing the
b1/b2 per-source `quit =>` registrations broke every path that reaches the
tap's `quit =>` handler *without* going through `call_supply_tap` — i.e. a
`Supplier`'s own `"quit"` method (both the immutable and mutable native
handlers in `native_supplier_methods.rs`) and `invoke_done_callback_or_quit`
(the existing LAST-phaser-die-to-quit conversion in
`native_supply_methods.rs`, pinned by
`t/whenever-last-phaser-die-converts-to-quit.t` and
`t/promise-supply-nested-quit-breaks.t`). Both used to find the downstream
handler because the old b1 registration happened to live on the *source's*
own `supplier_id` — the same id these two call sites already had in hand.
Fixed with a new helper, `take_supplier_quit_callbacks_via_group`
(`src/runtime/native_methods/state_supplier.rs`): drain the source's own
`supplier_id` first (still correct for a direct `.tap(quit => ...)` with no
`whenever` involved), then also drain via `supplier_serialize_group(sid)` —
the source→emitter link `b1` already records for a different reason (the
"only one whenever handler at a time" lock, ADR-0028). All four call sites
that used to read `take_supplier_quit_callbacks(sid)` for a *whenever
source's own* unhandled quit now go through this helper instead; the two
plain-tap emit-dispatch fallbacks (which have no serialize group and were
already correct) are untouched.

Tests: `t/supply-whenever-body-die-quits-block.t` (new — the ticket's repro,
probe3 B/C, probe6 F/G, all cross-checked against `raku` first) plus the full
existing `t/supply-*.t` / `t/whenever-*.t` / `t/react-*.t` / `t/promise-supply-*.t`
suites (113 files, 522 tests, all green) and every whitelisted `roast/S17-*`
file on a release build (Files=58+15+..., all green). `cargo test` (852 unit
tests) and `cargo clippy -- -D warnings` are clean.

### Slice 2 (Decision B) and Slice 3 (retire the ticket): not started

`supply_get_values` still replays; `replay_cold_whenever_capture` and
`replay_static_whenever_promise` are unchanged. Defect B in the ticket
(`todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`) is therefore
still open, and the ticket is not retired. A future session can pick up Slice
2 directly from the "Mechanism" section above — nothing in Slice 1 changed
its shape.
