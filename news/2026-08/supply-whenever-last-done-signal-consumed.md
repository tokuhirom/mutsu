# `LAST done;` in a tapped supply block no longer escapes as an empty runtime error (or double-fires the downstream done)

Found while re-verifying the (already-fixed) second-Preserving-instance
body-blob ticket: the `LAST done;` idiom — the exact shape
`Cro::HTTP2::GeneralParser` uses (`whenever $in { ...; LAST done; }`) —
was broken on the plain `.tap` dispatch path. Minimal repro:

```raku
my $in = Supplier.new;
my $out = supply {
    whenever $in.Supply -> $v { emit $v; LAST done; }
};
$out.tap: -> $m { say "tapped $m" }, done => { say "done cb" };
$in.emit(3);
$in.done;          # aborted the whole process: "Runtime error: " (empty)
```

Two defects, both in the done-callback dispatch:

1. **The control signal escaped.** The supply-body desugar rewrites the
   phaser's `done` to `$emitter.done()` followed by a `SupplyBodyDone`
   control signal. A whenever *body*'s `done` is consumed by
   `call_supply_tap`, but LAST phasers are dispatched through
   `invoke_done_callback` via a bare `call_sub_value`, and
   `invoke_done_callback_or_quit` deliberately re-propagated
   done-signals "so the supply machinery consumes it" — on the inline
   producer-`.done()` path there is no such machinery, so the signal
   surfaced at the producer's call site as an empty runtime error and
   killed the process (mainline) or the emitting worker thread.
2. **Stopping there double-fired the downstream done.** Once the signal
   is absorbed, the phaser's own `$emitter.done()` has already delivered
   the downstream `done =>` (via the `__SupplyOnDemandComplete` marker,
   with upstream teardown); the triggering source's remaining
   done-callback chain still held the whenever done-group marker, which
   delivered the same downstream done a second time.

Fix, in `invoke_done_callback` (`src/runtime/native_supply_methods.rs`):
plain callbacks are now dispatched like `call_supply_tap` dispatches the
whenever body — the callback's stamped emitter is pushed as the active
supply emitter, the react-done handler guard is held for stamped
callbacks, an escaping `SupplyBodyDone` is absorbed, and a raw
react-done signal is consumed by calling `done` on the stamped emitter.
The function now returns whether the callback completed the enclosing
supply itself (detected via the emitter's done-call count, the same
discriminator `run_on_demand_body` uses), and both the
`__SupplyDoneChain` loop and `invoke_done_callback_or_quit`'s callers
stop the rest of that source's done batch then — a supply terminates
via either `done` or `quit`, never both, and only once.

Verified against `raku` on: single-whenever `LAST done`; `LAST done`
with a sibling whenever still open (the GeneralParser shape — the
supply must complete anyway); `LAST emit` promise resolution (the
`Cro::MessageWithBody.body-blob` shape); die-in-LAST still routing to
the `quit =>` handler; and done-in-body single-fire. Pinned by
`t/supply-whenever-last-done.t`. All 127 supply/whenever/react `t/`
files and the vendored Cro HTTP2 parser/serializer suites
(`http2-request-parser` 61/61 etc.) stay green.
