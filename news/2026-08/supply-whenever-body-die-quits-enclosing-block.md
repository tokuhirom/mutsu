# A `whenever` body's `die` now quits the enclosing `supply` block, everywhere

Implemented ADR-0031 Decision A / Slice 1
(`docs/adr/0031-supply-quit-ownership-and-cold-source-tapping.md`): a `die`
inside a `whenever` body now belongs to the `supply` block it is written in,
not to whichever upstream source happened to dispatch the callback that ran
it.

Previously, mutsu attached a tap's `quit =>` handler to whichever *upstream*
object each of the four `whenever`-source branches in the `"tap" | "act"`
dispatch arm happened to have in hand. Two of the four (a direct
supplier-backed source and a channel-backed source) worked, but only by
accident: the die happened to unwind through the emit-dispatch code that
routes a failed tap callback to the very supplier the handler was attached
to. The other two — a `whenever` chained onto another on-demand supply, and a
`whenever Promise.in(...)` registered from *inside* another whenever's body —
had no such coincidence, so the die simply vanished: the enclosing supply
kept running as if nothing had happened.

The fix makes the enclosing supply block's own emitter own `quit`, the same
way it already owns `emit`, `done`, and `CLOSE`:

- `call_supply_tap` (the chokepoint every whenever/tap callback dispatch
  already runs through) now converts a *stamped* callback's non-control `Err`
  into `$emitter.quit($reason)` via the canonical `Supplier."quit"` — the
  dual of the `done`-absorption it already did. A plain, unstamped
  `.tap({ die ... })` is untouched: only a callback that is literally a
  `whenever` body takes this path.
- The tap's `quit =>` handler is now registered once, on the block's own
  emitter, before the four whenever-source branches run — instead of
  per-source, and instead of not at all for the previously-uncovered chained
  branch.
- The nested `whenever <Promise>` case (registered from inside another
  whenever's body, after the enclosing block's own synchronous run is over)
  now routes its body through `call_supply_tap` too, so its die converts the
  same way.

Fixing this surfaced a second, related gap: once the tap's `quit =>` handler
moved off each source's own `supplier_id`, two existing mechanisms that used
to find it *there* — a `Supplier`'s own `.quit()` method (a genuine external
source quit, not a body die) and the existing LAST-phaser-die-to-quit
conversion — stopped reaching it. Both are fixed with a new
`take_supplier_quit_callbacks_via_group` helper that also checks the
serialize-group link the supplier-backed branch already records for an
unrelated reason (the "only one whenever handler at a time" lock).

Two negative pins keep the two decision points from bleeding into each other:
a body `die` does *not* run that same `whenever`'s own `QUIT` phaser (it goes
straight to the tap's `quit =>`), while a *source* quit does run it first, and
a handled one still suppresses the downstream `quit =>`.

New test: `t/supply-whenever-body-die-quits-block.t`, covering the originating
deep ticket's repro plus the ADR's probe3 B/C and probe6 F/G cases, all
cross-checked against `raku` first.

Decision B (`supply_get_values` taps and drains instead of replaying a cold
source) and the ticket's retirement are follow-up work — see the updated
`todo/deep/cold-supply-whenever-source-replayed-not-tapped.md` and ADR-0031's
"Outcome" section.
