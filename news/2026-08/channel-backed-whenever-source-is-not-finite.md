# A channel-backed `whenever` source no longer completes its supply at tap time

`Cro::HTTP::Middleware::Conditional` lets a request-side transform answer a
request early (the classic "403 without an `Authorization` header") and hands
the response-side transform the skipped response over a `Supplier` kept in
per-connection state:

```raku
my class SkipPipelineState {
    has Supplier $.early-responses .= new;
}
```

On mutsu that early response never arrived: the client's `await` resolved to
`Nil` and the *next* request on the server never reached the middleware at all.
This was filed as a deep ticket
(`todo/deep/supplier-taps-lost-across-a-connection-thread.md`) on the theory
that an `Instance`'s attribute map was being deep-copied across a thread
boundary, since a plain `.tap` registered on the pipeline-construction thread
never fired while one registered on the connection thread did, and both
`Supplier`s reported the same `.WHICH`.

The theory was wrong, and the identity evidence was a red herring. Both threads
did hold the same supplier: the tap registry is a process-global map keyed by
`supplier_id`, and the emit found the right entry. It found the entry with the
tap already **closed** — because someone had run `.done` on that `Supplier`
during pipeline construction, long before the first request. That someone was
the middleware's own response transform:

```raku
whenever $pipeline -> $response {
    emit $response;
    LAST $connection-state.early-responses.done;
}
```

## Root cause

When a `supply` block is tapped, `native_supply_mut`'s on-demand `tap` path runs
the block body and counts how many of the `whenever` sources it registered can
keep the supply open (`whenever_supplier_count`). If that count is zero the
block is finite, so its downstream `done` — and with it every `whenever`'s LAST
phaser — fires immediately at tap time.

The count recognised two kinds of source: `Supplier`-backed ones (the done-group
marker is registered on the supplier's done) and chained on-demand ones (the
marker is passed as the inner tap's done). It did not recognise the third kind:
a **channel-backed** live source, the shape every `IO::Socket::Async` listener
and every connection's incoming byte supply has. Those are driven by a reader
thread and end only when the channel signals `Done`, but they carry a
`supply_id` rather than a `supplier_id`, so they fell through the filter.

Every Cro pipeline bottoms out at a socket's incoming supply, so *every* supply
block in the chain looked finite and completed the instant it was tapped. The
`Conditional` middleware's LAST phaser therefore closed the early-response
`Supplier` before the connection had received a single byte.

## Fix

- `has_supply_channel(supply_id)` peeks the supply-channel registry without
  consuming the receiver, so the count can tell a genuinely channel-backed
  source from one whose channel has already been taken by another branch.
- `whenever_supplier_count` counts such sources, so the enclosing block keeps a
  done group and no longer fires a spurious `done` at tap time.
- Because the source now participates in the done group, the channel branch
  hands its reader thread the whenever's LAST phasers plus the group marker as
  the loop's `done` callback, and `run_supply_act_loop` dispatches that through
  `invoke_done_callback` (which understands group markers and
  `__SupplyDoneChain`, and falls through to a plain call for an ordinary
  callable). The completion that used to be spurious now happens at the right
  moment: when the channel actually ends.

The vendored Cro suite's `http-middleware.rakutest` gains four assertions —
subtest 3's "Got 403 response from middleware when no auth header" now passes in
full, including the `throws-like` `.response` check that never ran before.

Pinned by `t/supply-whenever-channel-source-is-live.t`.
