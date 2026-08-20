# `.map`/`.grep`/`.do`/`.flat` drop the `"scheduler"` attribute when deriving a Supply

**Status: ready for direct implementation.** Investigated and re-scoped
2026-08-20 against `main` (33f75a62f). This was filed under `todo/deep/` as a
large design problem needing a new shim subsystem; a probe patch disproved that
premise. The fix is eight lines at two sites, mirroring seven sites that
already do it. The design rationale, the measurements, and the one genuinely
architectural residue that is *not* part of this ticket live in
[ADR-0043](../../docs/adr/0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md).

## The bug

`.map`, `.grep`, `.do` and `.flat` applied to a `.schedule-on()`'d Supply still
deliver to the tap callback synchronously, on the emitting thread, which
deadlocks the shape ADR-0028 exists to fix:

```raku
my $supplier = Supplier.new;
my $inner = Promise.new;
my $done = Promise.new;
my $inner-status;

$supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(-> $v { $v }).tap: -> $v {
    await Promise.anyof($inner, Promise.in(3));
    $inner-status = $inner.status;
    $done.keep(True);
};
start {
    $supplier.emit('x');
    $inner.keep(True);
};
await Promise.anyof($done, Promise.in(5));
say "inner-status: $inner-status";   # raku: Kept.  mutsu: Planned (deadlocked).
```

`.grep(-> $v { True })`, `.do(-> $v { })` and `.flat` reproduce identically.

## Root cause

ADR-0028 Slice 2 fixed the *deferred-registration* derived operators (`.lines`,
`.words`, `.unique`, `.elems`, `.produce`, `.head`, `.classify`,
`.categorize`) by copying `"scheduler"` into the fresh downstream attribute map
alongside the `supplier_id` they already copied, so the eventual `.tap()` on
the derived Supply reaches the `"tap" | "act"` chokepoint with the scheduler
visible and `wrap_scheduled_callbacks` defers delivery.

The four operators here build the same kind of fresh downstream attribute map
and simply omit `"scheduler"`. They were classified as a different category
because their *transform* registers immediately rather than at tap time — but
the transform registration is not what the deadlock runs on. The **user's tap**
registers at `.tap()` time through the ordinary chokepoint, exactly as in the
fixed category; it just never sees a scheduler.

Confirmation that the chokepoint is fine and only the attribute is missing:
`$supplier.Supply.map(-> $v { $v }).schedule-on(ThreadPoolScheduler.new).tap(...)`
— the same chain with the scheduler applied *last*, so the tapped Supply carries
the attribute itself — already prints `Kept` on `main`.

## The fix

Two sites, three lines each, copied verbatim from the seven existing
copy-forward sites (e.g. `native_supply_dispatch.rs:500-503`):

1. `src/runtime/methods_supply_dispatch.rs` — `make_live_transform_supply`
   (shared by `.map`, `.grep`, `.do`), in the `new_attrs` block just before
   `Value::make_instance`:

```rust
if let Some(scheduler) = attributes.get("scheduler") {
    new_attrs.insert("scheduler".to_string(), scheduler.clone());
}
```

2. `src/runtime/native_supply_dispatch.rs` — the `"flat"` arm's live branch
   (the one that calls `register_supplier_flat_tap`), same three lines in its
   `new_attrs` block.

## Verification already done (probe patch, debug build, `main` 33f75a62f)

| operator | main | + fix | raku |
|---|---|---|---|
| `.map`  | `Planned` | `Kept` | `Kept` |
| `.grep` | `Planned` | `Kept` | `Kept` |
| `.do`   | `Planned` | `Kept` | `Kept` |
| `.flat` | `Planned` | `Kept` | `Kept` |

Emission order survives the pump: `1,2,3` through
`.schedule-on(ThreadPoolScheduler.new).map(* * 10)` delivers `10 20 30`.

Green under the probe patch, unchanged: `t/supply-schedule-on.t`,
`t/supply-schedule-on-defer.t`, `t/supply-schedule-on-defer-nested-whenever.t`,
`t/schedule-on-whenever-env.t`, `t/supply-interval-scheduler.t` (21 tests) and
`roast/S17-supply/{schedule-on,map,grep,flat}.t` (37 tests).

## What the implementer still owes

- A new pin, `t/supply-schedule-on-defer-transform-ops.t`: the four operators
  above, an emission-order case, a `CurrentThreadScheduler` negative case
  (delivery must stay synchronous there), and a `Tap.close` cascade case.
  **Cross-check every case against real `raku` first** — ADR-0028's deep ticket
  warns, correctly, that plausible-looking simplifications of these repros
  often fail to reproduce.
- `make test` locally; full `make roast` on CI (this touches Supply dispatch).

## Explicitly NOT in this ticket

Blocking work inside the **transform callable itself** (as opposed to inside
the tap callback) still runs on the emitting thread and still deadlocks, with
or without this fix — raku `Kept`, mutsu `Planned`. That is a separate
placement question (mutsu puts the single deferral hop at the final `.tap`;
Rakudo puts it at the scheduled-source boundary), decided and deliberately
deferred as Decision 2 of ADR-0043, with its trigger and repro recorded there.
Do not widen this ticket into it.
