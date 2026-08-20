# ADR-0043: The scheduled-delivery hop belongs to the *tapped* Supply — every derived live operator carries `"scheduler"` forward

- Status: Proposed (design complete; Decision 1 verified by probe patch and
  ready for direct implementation, Decision 2 deliberately deferred behind a
  recorded trigger)
- Date: 2026-08-20
- Related: [ADR-0028](0028-supply-schedule-on-deferred-tap-delivery.md) (the
  `schedule-on` deferral mechanism this ADR places; its §4 "Propagation
  semantics" explicitly left this question open for a Slice-2 probe),
  [ADR-0020](0020-shared-worker-pool.md) (the pool the drain pump borrows a
  worker from — the resource cost that decides Decision 2),
  [ADR-0008](0008-push-based-supply-event-delivery.md)
- Supersedes the design sketch in
  `todo/deep/schedule-on-live-transform-operators-bypass-deferral.md`, whose
  central premise is disproved below; the residual ticket is
  `todo/tickets/scheduled-supply-derived-transform-ops-drop-scheduler-attr.md`

## Context

### What ADR-0028 left open

ADR-0028 made `Supply.schedule-on($scheduler)` genuinely defer delivery by
wrapping the emit/done/quit callbacks at the single `"tap" | "act"`
registration chokepoint. Its §4 recorded a *guess* about how far that
scheduling should propagate through derived operators, and explicitly declined
to widen blind:

> Whether mutsu's derived-operator *internal* registrations on a scheduled
> source need the same wrapping is observable only through blocking/thread
> identity — Slice 2 probes it against `raku` rather than widening blind.

Slice 2 then split the derived operators into two categories and fixed one:

- **Category 1 — deferred registration** (`.lines`, `.words`, `.unique`,
  `.elems`, `.produce`, `.head`, plus `.classify`/`.categorize`): these build a
  fresh downstream attribute map and defer actual tap registration until the
  caller taps the derived Supply — which *does* reach the chokepoint. Fixed
  mechanically, by copying `"scheduler"` into that fresh map alongside the
  `supplier_id` that was already copied (7 sites do this today).
- **Category 2 — immediate registration** (`.map`, `.grep`, `.do` via
  `make_live_transform_supply`; `.flat` via `register_supplier_flat_tap`):
  these register a transform tap on the *source*'s `supplier_id` at
  `.map()`/`.grep()`/`.do()`/`.flat()` call time. Deferred to a deep ticket on
  the grounds that this category is *architecturally different* and needs a new
  synthesized-shim class (`__ScheduledTransformApply`) mirroring
  `__ScheduledTapPump`.

### The category-2 premise is wrong (measured, 2026-08-20)

The deep ticket's reasoning is that category 2 has "no attrs map to thread
`"scheduler"` through, and no existing tap-callback registration point to route
through `wrap_scheduled_callbacks`". The first half is false. Category 2
registers *two independent things*, and the ticket conflates them:

1. the **transform application** (source `supplier_id` → downstream
   `supplier_id`), which is indeed registered immediately, in Rust, via
   `register_supplier_transform_tap` / `register_supplier_flat_tap`; and
2. the **user's tap** (downstream `supplier_id` → the user callback), which is
   registered later, at `.tap()`/`.act()` time, through the ordinary
   chokepoint — exactly like category 1.

`make_live_transform_supply` (`src/runtime/methods_supply_dispatch.rs:528`)
builds a fresh `new_attrs` map for the downstream Supply right there, and the
`"flat"` arm (`src/runtime/native_supply_dispatch.rs:738`) does the same. Both
carry `supplier_id` forward and both simply omit `"scheduler"`. There is a
perfectly ordinary attribute map to thread the scheduler through; it was just
missed in the Slice-2 sweep because the category was classified by *when the
transform* registers rather than by *when the user tap* registers, and the user
tap is what the deadlock actually runs on.

The reported deadlock is caused by (2), not (1). Two measurements settle it,
both against current `main` (33f75a62f) with a debug build:

**Measurement A — reordering the chain.** Putting `.schedule-on` *after* the
transform, so the tapped Supply carries `"scheduler"` itself, already works
today:

```
sched-then-map : raku Kept  / mutsu Planned   <- the filed bug
map-then-sched : raku Kept  / mutsu Kept      <- chokepoint deferral works fine
```

**Measurement B — a probe patch.** Copying `"scheduler"` forward at the two
sites (8 lines, no new types, no new registry, mirroring the 7 category-1 sites
verbatim) flips the entire reported operator set:

| operator | main | + copy-forward | raku |
|---|---|---|---|
| `.map`  | `Planned` | `Kept` | `Kept` |
| `.grep` | `Planned` | `Kept` | `Kept` |
| `.do`   | `Planned` | `Kept` | `Kept` |
| `.flat` | `Planned` | `Kept` | `Kept` |

Emission order survives the pump (`10 20 30` for `1,2,3` through
`.schedule-on(ThreadPoolScheduler.new).map(* * 10)`), and every existing pin
stays green under the probe patch: `t/supply-schedule-on.t`,
`t/supply-schedule-on-defer.t`,
`t/supply-schedule-on-defer-nested-whenever.t`, `t/schedule-on-whenever-env.t`,
`t/supply-interval-scheduler.t` (21 tests), plus
`roast/S17-supply/{schedule-on,map,grep,flat}.t` (37 tests).

So the `__ScheduledTransformApply` subsystem the deep ticket sketches is not
needed to close the filed repro. It buys something narrower — see below.

### The real residue: *where* the hop sits

There is a genuine remaining difference, and it is not the one the ticket
describes. With `"scheduler"` copied forward, blocking work inside the **tap
callback** is deferred, but blocking work inside the **transform callable
itself** is not:

```raku
$supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(-> $v {
    await Promise.anyof($inner, Promise.in(3));   # blocks INSIDE the mapper
    $inner-status = $inner.status; $v
}).tap: -> $v { $done.keep(True) };
start { $supplier.emit('x'); $inner.keep(True) }
```

raku prints `Kept`; mutsu prints `Planned` both on `main` and with the
copy-forward patch. The mapper runs synchronously on the emitting thread, so
the `start {}` block cannot reach `$inner.keep(True)`.

The reason is a placement difference, not a missing mechanism. Both
implementations perform exactly **one** deferral hop per chain; they put it at
opposite ends:

- **Rakudo** derives `.map` as `supply { whenever self -> \v { emit(f(v)) } }`.
  The `whenever` is itself a tap on the *scheduled* supply, so the hop happens
  at the **source boundary**: `f`, any further stages, and the final user
  callback all run on the scheduler thread. A second `.map(g)` taps an
  ordinary, unscheduled supply block, so it adds no hop.
- **mutsu with copy-forward** applies every transform synchronously at the emit
  site and puts the hop at the **final `.tap`**: `f`, `g` run on the emitting
  thread, only the user callback runs on the pump thread.

Same hop count, same per-tap FIFO ordering, different thread for the user's
transform callables. This is the honest statement of what ADR-0028 §4 left
open, and it is a decision about *placement*, not a missing subsystem.

(A probe of `.produce` — a category-1 operator that also takes a user callable
— was inconclusive: the binary callable never ran in either implementation for
the shape tried, so the residue is confirmed only for `make_live_transform_supply`'s
`.map`/`.grep`/`.do`. Whether it generalizes to the other callable-taking
derived operators is unmeasured and should be probed before being assumed.)

## Decision

### Decision 1 — the hop stays at the tap chokepoint; the attribute travels uniformly

`"scheduler"` is an attribute of a Supply *value*, and it must be carried
forward by **every** derived live operator that builds a fresh downstream
attribute map — with no category-1/category-2 distinction. The
`"tap" | "act"` chokepoint remains the single place deferral is decided, per
ADR-0028's core finding.

Concretely: add the same three-line copy-forward the 7 category-1 sites already
carry to the two sites that lack it —
`make_live_transform_supply` (`methods_supply_dispatch.rs`, covering `.map`,
`.grep`, `.do`) and the `"flat"` arm (`native_supply_dispatch.rs`).

This is not a band-aid ahead of a "real" fix. It removes a *special case*: nine
derived-operator sites, seven of which propagate the attribute and two of which
silently drop it, become nine that all do. By the repo's gain/risk definitions
that uniformity is the gain, and there is no flakiness surface — the change is
attribute plumbing on a value, verified deterministically by the table above.

Implementation is a ticket, not a campaign:
`todo/tickets/scheduled-supply-derived-transform-ops-drop-scheduler-attr.md`
carries the exact diff and the verification matrix. It needs one new pin
(`t/supply-schedule-on-defer-transform-ops.t`, four cases mirroring the
measured matrix, each cross-checked against real `raku` first per ADR-0028's
own warning that plausible simplifications don't always reproduce).

### Decision 2 — moving the hop to the source boundary is deferred behind a trigger

Making transform callables run on the scheduler (Rakudo's placement) is **not**
adopted now. When it is adopted, the correct shape is *not* the deep ticket's
per-operator shim registered in addition to the tap-time pump — that would
create two hops per chain, and N hops for an N-stage chain. It is:

> When a live-transform operator is built on a source carrying `"scheduler"`,
> register the transform application itself through `wrap_scheduled_callbacks`
> on the source, and **do not** carry `"scheduler"` forward to the downstream
> Supply — the deferral has already happened, and everything downstream (later
> stages and the final user callback) inherits the pump thread for free.

That is one hop, at Rakudo's position, and it makes Decision 1's copy-forward
redundant for exactly these operators (which is why the two are alternatives,
not phases of one change: adopting Decision 2 means *removing* Decision 1's two
copy-forwards, not building on them).

It is deferred because the cost lands in the wrong place:

- **A pump is created at `.map()` call time, whether or not anyone ever taps
  the result.** A pump parks a pool worker on a blocking `recv` for the
  supply's lifetime (ADR-0028's own first listed risk, ADR-0020 slice-3 shape).
  Under Decision 1 a pump exists only when a tap exists — strictly the better
  resource profile, and the one that matches the chokepoint philosophy of
  "delivery is decided at registration time".
- **The observable it buys is narrow and currently unexercised.** It changes
  behaviour only for a *blocking* user transform callable on a scheduled
  supply. The Cro suites — the campaign's yardstick — are 35/35 and 9/9 without
  it, and no test in `t/` or the roast whitelist exercises the shape.

**Trigger to revisit (record it, don't re-derive it):** a real workload — a Cro
or battery suite, or a roast test — that blocks inside a transform callable on
a `.schedule-on()`'d supply. The pin to start from already exists as a probe:
`tmp/schedule-on-blocking-mapper.raku` in this ADR's investigation (`raku`
`Kept` / mutsu `Planned`). Until then, this is a documented, measured, bounded
difference from Rakudo, not an unknown.

If mutsu ever grows a first-class supply-block serialization lock, ADR-0028's
alternative 4 (derive these operators as real supply blocks) subsumes Decision
2 entirely and is the better retirement path; note this ADR in that campaign.

## Alternatives considered

1. **The deep ticket's `__ScheduledTransformApply` shim class, as filed.**
   Rejected on evidence: it was proposed to fix a repro that 8 lines of
   attribute plumbing fix (measurement B), so its cost — a new native class, a
   `ClassDef` entry, a dispatch arm, pump-lifetime threading onto the
   downstream handle, and a map/grep/do × scheduled/unscheduled × close-cascade
   test matrix — buys only the blocking-transform residue. And as filed it
   registers the shim *in addition to* leaving the tap-time path intact, which
   double-hops. The salvageable idea is its placement insight, preserved as
   Decision 2.
2. **Copy `"scheduler"` forward AND route the transform through the pump.**
   Rejected: two pumps, two parked workers, two hops per chain, for no
   observable gain over Decision 2 alone.
3. **Per-stage re-cue (a pump for every derived operator in the chain).**
   Rejected — Rakudo does not do this (ADR-0028 §4 established it), the worker
   cost is linear in chain length, and the added latency is user-visible.
4. **Document the whole thing as an accepted difference and change nothing.**
   Rejected for Decision 1: the filed repro is a genuine deadlock, the fix is
   eight lines, and leaving two of nine sites inconsistent is precisely the
   kind of silent special case that made this gap survive the Slice-2 audit in
   the first place. Accepted, explicitly and with a trigger, for Decision 2.
5. **Classify derived operators by "when the transform registers" (the
   Slice-2 model).** Rejected as the wrong axis — it is what produced the
   false category-2 conclusion. The load-bearing question is *when the user's
   tap registers*, which is `.tap()` time for every derived operator.

## Acceptance criteria (Decision 1)

1. `.map`, `.grep`, `.do`, `.flat` applied after `.schedule-on(ThreadPoolScheduler.new)`
   all report `Kept` for the deep ticket's blocking-tap-callback repro, matching
   `raku`; pinned in `t/supply-schedule-on-defer-transform-ops.t` with each case
   cross-checked against real `raku` output first.
2. Emission order is preserved through the pump (`1,2,3` → `10 20 30`), pinned
   deterministically via a kept Promise in the `done` callback — no sleeps, no
   timing assertions.
3. `CurrentThreadScheduler` stays synchronous through the derived operators
   (the §1 fork in `wrap_scheduled_callbacks` already guarantees this; pin it
   negatively so a future change to the fork cannot silently defer it).
4. The five existing schedule-on pins and
   `roast/S17-supply/{schedule-on,map,grep,flat}.t` stay green — verified
   already under the probe patch, must be re-verified on the real change.
5. `make test` locally; full `make roast` delegated to CI (this touches Supply
   dispatch, so a local subset is not sufficient). `S17-*` failures get the
   flaky-triage protocol, not a shrug.
6. On completion, `git mv` the ticket to a `news/2026-08/` entry and update
   this ADR's Status with the outcome, per `docs/adr/README.md`.

## Risks

- **Behaviour change for existing scheduled chains.** Any code tapping a
  `.map`/`.grep`/`.do`/`.flat` derived from a `ThreadPoolScheduler`-scheduled
  supply moves from synchronous to pump-thread delivery — by design (raku
  defers there too), and the same change ADR-0028 already shipped for direct
  taps and category-1 operators. The inventory found no test relying on the
  synchronous behaviour; all existing schedule-on coverage uses
  `CurrentThreadScheduler`, which is unaffected by construction.
- **Cross-thread tap callbacks.** Callbacks that previously ran on the emitting
  thread now run on a pool worker under `clone_for_thread` — ADR-0028's second
  listed risk, unchanged in kind and already the most heavily exercised shape
  of the Cro campaign.
- **One pump per scheduled derived tap.** Same profile as ADR-0028's existing
  pumps; `Tap.close` reclaims it through the path Slice 2 already built. Worth
  an explicit close-cascade case in the new pin.
- **Decision 2 staying deferred.** The residue is a real, measured divergence
  from Rakudo. The mitigation is that it is *recorded with its repro and its
  trigger* rather than rediscovered — an unmeasured gap is the risk; a measured
  and bounded one is a known cost.
