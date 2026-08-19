# `supply_get_values` still replays a cold `whenever` source instead of tapping it

Found 2026-07-25 in Test::Scheduler (`TODO_dist` T-037). Narrowed 2026-07-25
after the *nested* half of it was fixed (pin `t/supply-nested-whenever-promise.t`).
**Re-measured 2026-08-19 against `530ccf7dd`: half of the original analysis had
evaporated, and the surviving half was a different bug than the title
suggested.** The design that replaced this file's original root-cause section
is [ADR-0031](../../docs/adr/0031-supply-quit-ownership-and-cold-source-tapping.md).

**Update 2026-08-19: Defect A (quit ownership) is fixed — ADR-0031 Slice 1
shipped.** The ticket's own repro (below, previously mis-delivering a third
`'badger'` and never quitting) now matches `raku` exactly (`["badger",
"badger"]` / `died=True`), pinned by `t/supply-whenever-body-die-quits-block.t`.
That fix also uncovered and closed a second gap in the same area: a
`Supplier`'s own `.quit()` (a genuine *source* quit, not a whenever body die)
stopped reaching a tap's `quit =>` handler once the per-source registration
that used to carry it was removed — see the ADR's "Outcome" section for the
`take_supplier_quit_callbacks_via_group` fix and its own pins
(`t/whenever-last-phaser-die-converts-to-quit.t`,
`t/promise-supply-nested-quit-breaks.t`).

**What remains open, and what this file now tracks, is Defect B only:**
`supply_get_values` (ADR-0031 Decision B, Slice 2) still replays a cold
on-demand source synchronously instead of tapping it, so it silently drops a
live inner subscription. Read ADR-0031's "Mechanism" section (Slice 2) before
starting; nothing about that plan changed during Slice 1.

## Repro (Defect B — probe5 case E from the ADR)

```raku
my $supE = Supplier.new;
my $srcE = supply { whenever $supE.Supply -> $v { emit $v } }
my $outE = supply { whenever $srcE   -> $v { emit $v } }
start { sleep 0.05; $supE.emit('e1'); $supE.emit('e2'); $supE.done }
say $outE.list;     # raku: (e1 e2)    mutsu: ()
```

A related symptom on the same family's sibling path: `await` on a supply whose
`whenever` source is a cold on-demand supply returns `Nil` where raku returns
the last emitted value — `supply_promise_on_demand` still finishes through
`replay_static_whenever_promise` in that shape.

## Root cause (detailed in ADR-0031 Decision B)

`supply_get_values` (`src/runtime/supply_promise.rs:239`, with
`replay_cold_whenever_capture` `:676` and `replay_static_whenever_promise`
`:789`) still replays a cold source synchronously instead of tapping it. It
drops any live inner subscription (`if is_live { continue; }`, `:328-330`), so
`supply { whenever <cold supply whose own whenever is on a live Supplier> }`
`.list`s to `()` where raku gives the values. This affects only the
`supply_get_values` family (~20 combinator/`.list`/`.wait` call sites); the
`.tap`/`.act` and react/`.Promise` paths already chain real taps (and, as of
Slice 1, correctly quit too).

## Affected files

- `src/runtime/supply_promise.rs` — `supply_get_values`,
  `replay_cold_whenever_capture`, `replay_static_whenever_promise`.
- `src/runtime/methods_call_helpers.rs` — `supply_list_values`, the `.list`
  feeder ADR-0031 names as Slice 2 step 1's pin.
- `src/runtime/native_supply_dispatch.rs` — the `.sort` / `.squish` / `.head`
  / `.flat` / `.produce` / `.batch` / `.rotor` / `.rotate` / `.comb` / `.snip`
  / `.minmax` / `.zip` / `.start` / `.Channel` combinators (Slice 2 step 2).
- `src/runtime/supply_transform.rs` — `.throttle` / `.stable` (Slice 2 step 2).

## Why it is large

Replacing the replay family changes ~20 call sites from a synchronous pull to
a bounded drain, with a real hang risk when the producer runs on the calling
thread (the deadlock class ADR-0028 documents) — ADR-0031's "Guarding against
a new hang" subsection is the mitigation plan, and Slice 2 step 1 must probe
that shape explicitly before the combinators follow.

## Impact

`Test::Scheduler` (`TODO_dist` T-037): `t/synopsis.rakutest` and
`t/virtualized-time.rakutest` may still exercise this shape via `.list`/`.wait`
on a supply built from a cold on-demand `whenever` source; re-check their
counts once Slice 2 lands. Cro's body coercions ride on the same
`supply_get_values` family.
