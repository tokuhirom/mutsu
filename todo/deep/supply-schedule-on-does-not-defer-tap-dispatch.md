# `Supply.schedule-on($scheduler)` does not actually defer tap delivery — it is a near no-op for a plain `ThreadPoolScheduler`

## Design (2026-08-12)

Design complete: `docs/adr/0028-supply-schedule-on-deferred-tap-delivery.md`
(Status: Proposed). Wraps `tap_cb`/`done_cb`/`quit_cb` at the single
`"tap" | "act"` registration chokepoint (`native_supply_mut_methods.rs`)
rather than any of the ~33 emit call sites, with a scheduler-kind fork
(`CurrentThreadScheduler` stays synchronous; `ThreadPoolScheduler` gets a
serialized per-tap drain on the existing worker pool via
`supply_event_channel`/`run_supply_act_loop`, ADR-0008/ADR-0020 primitives;
any other `Scheduler` routes through its own `.cue`). Three implementation
slices (chokepoint wrap; audit of paths that bypass the tap arm; Cro
verification) plus acceptance criteria and risks are in the ADR — read it
before implementing, not just this summary.

## Symptom (superseded diagnosis of `preserved-tap-chain-loses-body-when-terminated-by-connection-close.md`)

That ticket recorded two `http-response-parser.rakutest` failures ("Response
with body terminated by close of connection", "Connection close with
incomplete body throws") and speculated the break was somewhere in
`Supplier::Preserving`/`preserve()`. Both hypotheses (`Supplier::Preserving`
itself, and `ResponseParser.transformer`'s own body shape) were minimized and
refuted — see the investigation trail below. The real root cause is a
different, more general mechanism: **`Supply.schedule-on()` does not actually
reschedule tap delivery onto the given scheduler.**

## Root cause

`Supply.schedule-on(scheduler)` (`src/runtime/native_supply_dispatch.rs:457`)
only stashes the scheduler value into a `"scheduler"` attribute on a cloned
Supply instance and resets its `"taps"` list — it does not change how a value
delivered to this supply reaches its tap callback. Grepping every consumer of
that `"scheduler"` attribute
(`src/runtime/native_supply_mut_methods.rs:144`) shows it is read in exactly
one place: to wire a `Supply.interval`-style periodic timer through
`$scheduler.cue(&code, :every, :in)` (`cue_scheduler_interval`). For every
other kind of Supply (in particular a plain on-demand `supply { ... }` block,
which is what `Cro::HTTP::ResponseParser.transformer` returns), a value
emitted upstream still reaches `.tap()`'s callback **synchronously, on the
emitting call stack** — exactly as if `.schedule-on()` had never been called.

Real Raku's `.schedule-on()` genuinely reschedules delivery via the given
scheduler (for a `ThreadPoolScheduler`, onto a worker-pool thread), so the
thread that produced the emission (e.g. one running inside a `start {...}`
block) returns immediately and can go on to do its next scheduled thing (e.g.
call `.done()` on a `Supplier`) concurrently with the tap callback running
elsewhere. Mutsu's `.schedule-on()` not doing this collapses two
independent-in-Raku executions back onto one call stack, which changes
program behavior whenever the tap callback does something blocking.

## Confirmed repro chain (Cro-independent)

`Cro::HTTP::ResponseParser`'s test helper `parses()` does:

```raku
$testee.transformer($fake-in.Supply).schedule-on($*SCHEDULER).tap:
    -> $response {
        pass $desc;
        for @checks.kv -> $i, $check { ok $check($response), ... }
        ...
    };
start {
    $fake-in.emit(test-response-to-tcp-message($test-response, :$body-blob));
    $fake-in.done();
}
```

One of `@checks` is `*.body-text.result eq "..."` — a **blocking** call.
Minimal reproduction (no Cro import, `tmp/response-parser-repro.raku`-style,
confirmed against both the vendored `Cro::HTTP::ResponseParser` AND
independently isolated Cro-free code with debug `note()` calls inserted at
every hop):

1. The `start {}` thread calls `$fake-in.emit(...)`.
2. That call synchronously walks the whole downstream tap chain
   (`ResponseParser.transformer`'s `whenever $in {...}` body → `emit
   $response` → the outer `.tap()`'s callback) **because `.schedule-on()`
   did not actually defer it**.
3. The callback calls `.body-text.result`, which blocks the *same* thread
   waiting for `Cro::MessageWithBody.body-blob`'s `Promise(supply { whenever
   self.body-byte-stream {...; LAST emit } })` to resolve.
4. That resolution needs `$fake-in.done()` — the **next statement in the same
   `start {}` block**, which can never run because the thread executing it is
   stuck inside step 3's blocking wait. Deadlock (times out to the test's own
   `Promise.in(10)` guard, producing "Response parser failed to emit a HTTP
   response").

Confirmed this is genuinely what `.schedule-on()` is supposed to prevent:
running the **same** minimal repro through real `raku` **without**
`.schedule-on()` reproduces the identical deadlock (`completed: Planned`,
`$fake-in.done()`'s effects never observed) — i.e. blocking inside a
synchronously-dispatched tap callback deadlocks in Raku too. With
`.schedule-on($*SCHEDULER)`, real Raku decouples the two and the test passes.
Mutsu deadlocks in both cases, because `.schedule-on()` never took effect.

## Why the two earlier candidate theories were refuted

- **`Supplier::Preserving`/`preserve()` in isolation**: a 3-level chain
  (`Supplier` → derived `supply { whenever $raw { .emit } }` → `preserve()`
  → plain `.tap()`) delivers all values and `done` correctly and promptly in
  mutsu. Not the break.
- **`ResponseParser.transformer`'s own body shape**: rebuilt the shape
  incrementally (class-body-private `sub preserve`, nested `whenever
  $cancellation` sibling registration via `fresh-message`, single-packet
  header+body-in-one-shot delivery, `Promise(supply { whenever
  <Supplier::Preserving.Supply> {...; LAST emit} })` fed by data buffered
  *before* the whenever taps it) — every incremental step reproduced fine.
  The deadlock only appears once a **blocking wait runs inside a
  `.schedule-on()`-wrapped tap callback** on the same thread that still needs
  to drive a later async event.

## Suggested fix direction (not implemented)

`.schedule-on()` needs to make tap delivery genuinely asynchronous relative
to the emitting call stack, at least for a plain `ThreadPoolScheduler`. Mutsu
already has a real worker pool used by `$scheduler.cue(&code)`
(`crate::runtime::worker_pool::submit`, see
`src/runtime/native_methods/scheduler.rs:374-387`) — the same primitive
`Supply.interval`'s scheduler wiring reuses. The likely shape of a fix:

1. Find the actual point(s) where a value reaches a registered tap callback
   for a Supply/Supplier chain (`register_supplier_tap` in
   `src/runtime/native_methods/state_supplier.rs:607` and friends — audit for
   *every* delivery path, not just the direct-Supplier one, since
   `.schedule-on()` can sit anywhere in a derived chain).
2. When the (innermost, or nearest-upstream) attributes carry a `"scheduler"`
   key pointing at a non-`CurrentThreadScheduler`, submit the tap-callback
   invocation via `worker_pool::submit(...)` instead of calling it inline,
   mirroring what `.cue()` already does for one-shot callbacks.
3. Decide the propagation rule for `.schedule-on()` sitting mid-chain (does a
   *derived* Supply built on top of a scheduled one inherit the scheduling,
   or only direct taps on the schedule-on'd Supply itself? Real Raku's docs
   describe it as affecting emit/done/quit delivery to `.tap()` callers of
   *that* Supply specifically — check `raku-doc/doc/Type/Supply.rakudoc`
   before designing).
4. This is cross-cutting (every Supply variant that can carry a `"scheduler"`
   attribute and every code path that invokes a tap callback), so it likely
   wants an ADR or at least a Fable design pass before implementation — the
   audit in step 1 alone is nontrivial given how many `register_supplier_*`
   variants exist (`state_supplier.rs`).

## Reproduce

```
DIST=$(echo /home/tokuhirom/work/mutsu-roast/tmp/cro-work/C_RO_CRO_HTTP_*)
INC=$(cat /home/tokuhirom/work/mutsu-roast/tmp/cro-work/inc-paths.txt)
timeout 60 /home/tokuhirom/work/mutsu-roast/target/release/mutsu $INC -I "$DIST/lib" -I "$DIST/t" t/http-response-parser.rakutest
```

Cro-free minimal repro demonstrating the same deadlock class (blocking call
inside a `.schedule-on()`-wrapped `.tap()` callback never lets a
concurrently-`start{}`-scheduled event run):

```raku
my $supplier = Supplier.new;
my $done = Promise.new;
$supplier.Supply.schedule-on($*SCHEDULER).tap: -> $v {
    # blocks; needs $supplier.done (below) to ever run to resolve
    my $inner = Promise(supply {
        whenever $supplier.Supply.grep(*== 'signal') { done }
    });
    await Promise.anyof($inner, Promise.in(3));
    say "inner status: ", $inner.status;  # stays Planned in mutsu
    $done.keep(True);
};
start {
    $supplier.emit('x');
    $supplier.emit('signal');
};
await Promise.anyof($done, Promise.in(5));
say "done: ", $done.status;
```

(This exact snippet has not been run standalone — build a fresh minimal
repro from it before starting the fix, per the investigation notes above:
several plausible-looking simplifications of this chain did NOT reproduce,
so re-verify any new simplification against real `raku` before trusting it.)

Requires the vendored Cro checkout under `tmp/cro-work/` from prior sessions
for the roast-adjacent repro (not part of this repo's tracked test suite —
Cro itself is intentionally not bundled, see `handoff-cro-next-steps` project
memory / `PLAN.md`). The Cro-free sketch above needs no vendored files.
