# A nested on-demand supply's LAST-phaser die now reaches the outer whenever's QUIT

`supply { whenever SOURCE -> $v { ...; LAST { die "reason" if ... } } }`
subscribed to by an outer `whenever` (in a `react {}` or another `supply
{}`) used to crash the whole drive loop raw when the inner `LAST` phaser
died, instead of surfacing as that outer subscription's own `QUIT { ... }`
handler — matching `raku`, which does deliver it to `QUIT`.

Root cause: the nested subscription flattened out of the on-demand
`supply { }` body's `whenever` (`build_react_subscriptions`'s
on-demand-callback branch, `vm/vm_react_loop.rs`) never read the outer
registration marker's own `quit_callbacks` (`items.get(3)`) at all — unlike
the direct supplier/channel branches a few lines above it, which already
did. Even reading it would not have been enough: nothing converted a
`LAST`-phaser die on the *inner* subscription into a `quit` signal on the
*outer* supply's own completion promise, so there was no event for a QUIT
handler to react to in the first place.

The fix threads the outer whenever's `quit_callbacks` through, tags the
inner live-source subscription with the owning on-demand supply's emitter
id (`ReactSubscription::emitter_supplier_id`, only when there is an actual
QUIT handler to route to — otherwise a die still propagates raw exactly as
before, with zero behavior change), and converts a `LAST`-phaser die on
that subscription into `supplier_quit(emitter_id, cause)` — symmetric with
the existing `supplier_done(emitter_id)` now also fired on that
subscription's *successful* completion. The Phase-2 `on_demand_done` poll
in `vm_react_subscriptions.rs` was extended to check the promise's
Kept/Broken status and dispatch to the outer subscription's QUIT vs LAST
callbacks accordingly, and a shadow subscription carrying those QUIT
callbacks (previously pushed only when a `closing => { ... }` callback
existed) is now also pushed whenever there is a QUIT handler, deferring its
own "done" until that promise actually resolves.

Verified independently against `raku` 2026.06 with a Cro-free minimal
repro (`supply { whenever Supplier.new.Supply {...} LAST {...} }` consumed
by an outer `react { whenever $inner {... QUIT {...}} }`): both the
QUIT-handled and no-QUIT-handler cases now match `raku`'s stdout exactly.
One `raku` quirk observed but deliberately **not** replicated: even after
its QUIT phaser handles the exception, `raku`'s `react` block still goes
on to crash with the same exception afterward (no further `say` runs, exit
1) — this looks like an unrelated internal reporting path in `raku`'s own
on-demand-supply implementation, not documented QUIT semantics, and
matching mutsu's own pre-existing behavior for a *handled* QUIT on a
direct (non-nested) `Supplier` (`t/whenever-quit-phaser.t`) — a handled
QUIT does not propagate further.

Full re-verification against the original motivating case
(`t/http-response-parser.rakutest` subtest 120, `Cro::HTTP::RawBodyParser::ContentLength`'s
`LAST`-phaser truncation error) is deferred — no local Cro checkout was
available this session — but the underlying mechanism this ticket
described is fixed and pinned independently of Cro.

New pin: `t/nested-on-demand-supply-last-die-reaches-quit.t`.
