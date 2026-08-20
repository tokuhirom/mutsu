# `.map`/`.grep`/`.do`/`.flat` on a scheduled Supply no longer drop the scheduler

`.map`, `.grep`, `.do`, and `.flat` applied to a `.schedule-on()`'d Supply
deadlocked when the user's tap callback blocked on an `await` that depended
on a sibling `start {}` statement:

```raku
$supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(-> $v { $v }).tap: -> $v {
    await Promise.anyof($inner, Promise.in(3));
    ...
};
```

raku delivers `Kept`; mutsu delivered `Planned` (deadlocked) because the tap
callback ran synchronously on the emitting thread instead of through the
deferred-delivery pump ADR-0028 built for `schedule-on`.

Root cause: `make_live_transform_supply` (shared by `.map`, `.grep`, `.do`,
in `src/runtime/methods_supply_dispatch.rs`) and the `"flat"` arm's live
branch (in `src/runtime/native_supply_dispatch.rs`) each build a fresh
downstream attribute map for the derived Supply, but neither copied the
`"scheduler"` attribute forward from the source Supply's attributes. Seven
other derived-Supply sites (`.lines`, `.words`, `.unique`, `.elems`,
`.produce`, `.head`, `.classify`, `.categorize`) already did this
copy-forward; these two were the odd ones out. Without the attribute, the
eventual `.tap()` on the derived Supply reaches the `"tap"|"act"`
chokepoint with no scheduler visible, so `wrap_scheduled_callbacks` never
defers delivery.

Fixed by adding the same `if let Some(scheduler) = attributes.get("scheduler")`
copy-forward (three lines each) to both sites. `.schedule-on(...).map(-> $v { $v }).schedule-on(ThreadPoolScheduler.new).tap(...)`
scheduling applied *after* the transform already worked, which was the
confirmation that the tap chokepoint itself was fine and only the attribute
was missing.

New regression pin: `t/supply-schedule-on-defer-transform-ops.t` covers
`.map`, `.grep`, `.do`, and `.flat` each not deadlocking, emission order
surviving the derived-transform + scheduled pump, a `CurrentThreadScheduler`
negative case (delivery correctly stays synchronous there — this is not an
unconditional-defer fix), and a `Tap.close` cascade through the derived
Supply. All cases were cross-checked against real `raku` before pinning, and
the test was verified to fail (4/7) with the fix reverted.

See ADR-0043 (`docs/adr/0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md`)
for the broader design context and the deliberately-deferred residue (Decision 2:
blocking work inside the transform callable itself, as opposed to inside the tap
callback, still deadlocks — a separate placement question, not part of this fix).
