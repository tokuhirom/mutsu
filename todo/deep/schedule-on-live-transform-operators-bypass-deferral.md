# `.map`/`.grep`/`.do` (and `.flat`) applied on a `.schedule-on()`'d Supply still deliver synchronously

## Root cause

ADR-0028 Slice 1 fixed `Supply.schedule-on($scheduler)` to genuinely defer
delivery, by wrapping the emit/done/quit callbacks at the single `"tap"|"act"`
chokepoint (`native_supply_mut_methods.rs`). Slice 2 (this campaign's current
work) audited the registration paths that bypass that chokepoint and found
two categories of gap:

1. **Deferred-registration derived ops** (`.lines`, `.words`, `.unique`,
   `.elems`, `.produce`, `.head`) just copy `supplier_id` forward into a
   fresh attrs map and set a mode flag; the actual tap registration happens
   later, when the user eventually calls `.tap()`/`.act()` on the derived
   Supply — which *does* hit the chokepoint. These were fixed by also
   copying the `"scheduler"` attribute forward (a small, low-risk, mechanical
   change, landed alongside the `whenever`-in-supply fix below).

2. **Immediate-registration live-transform ops** (`.map`, `.grep`, `.do` —
   all three funnel through `make_live_transform_supply` in
   `methods_supply_dispatch.rs` — and `.flat`, via `register_supplier_flat_tap`
   in `native_supply_dispatch.rs`) register a transform tap **immediately**,
   at `.map()`/`.grep()`/`.do()`/`.flat()` call time, directly on the
   *source*'s `supplier_id` via `register_supplier_transform_tap` /
   `register_supplier_flat_tap` — independent of if/when the caller later
   taps the *downstream* Supply. This bypass is architecturally different
   from category 1 and from the `whenever`-in-supply fix (both of which just
   needed to route through the existing `wrap_scheduled_callbacks` helper
   before calling the existing `register_supplier_tap`): the transform
   application itself is a Rust-side `TransformState` consulted synchronously
   at the ~33 `supplier_emit_callbacks` call sites
   (`SupplierEmitAction::TransformCall` → `handle_supply_transform_emit` in
   `native_supplier_methods.rs`), which never even looks up the "scheduler"
   attribute — it isn't visible from where the transform is applied.

This category-2 gap remains unfixed. Confirmed against real `raku` with a
Cro-free deadlock-shape repro (mirroring the Slice-1 repro, just with `.map`
spliced in before `.tap`):

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

Real Raku resolves the inner promise because `.map` is implemented in Rakudo
as `supply { whenever self -> \v { emit(f(v)) } }` — the map's *own* internal
`whenever` taps the scheduled source, so its body (and therefore its
`emit`, which drives the downstream Supply's delivery) runs deferred on the
scheduler. mutsu's `TransformState` mechanism has no equivalent hook.

## Affected files

- `src/runtime/methods_supply_dispatch.rs` — `dispatch_supply_map`,
  `make_live_transform_supply` (shared by Map/Grep/Do modes).
- `src/runtime/native_supply_dispatch.rs` — the `"do"` dispatch arm (line
  ~385, also calls `make_live_transform_supply`), the `"flat"` arm (line
  ~688, calls `register_supplier_flat_tap` directly — same category).
- `src/runtime/native_methods/state_supplier.rs` — `TransformState`,
  `register_supplier_transform_tap`, `register_supplier_flat_tap`.
- `src/runtime/native_supplier_methods.rs` — the ~2 `TransformCall`
  consultation sites, `handle_supply_transform_emit`.
- `src/runtime/native_supply_mut_methods.rs` — `wrap_scheduled_callbacks`
  (the Slice-1/Slice-2 shared helper this fix would need to route through),
  `__ScheduledTapPump` and its native methods (`state_scheduled_pump.rs`) —
  the reusable pump/cue shim machinery.

## Why it is large

A correct fix needs a new synthesized-callable shim (mirroring the existing
`__ScheduledTapPump` idiom) that a `.map`/`.grep`/`.do`/`.flat` call can
register via the *plain* `register_supplier_tap` (not `register_supplier_transform_tap`)
when the source carries `"scheduler"`, so `wrap_scheduled_callbacks` can wrap
it like any other tap callback and the existing pump/cue drain delivers it.
Sketch:

1. Add a new internal native class, e.g. `__ScheduledTransformApply`, holding
   `(mapper, mode, downstream_supplier_id)` — mirrors `__ScheduledTapPump`'s
   `(pump_id)` / `(scheduler, real_cb)` shape.
2. Give it a one-arg native method (e.g. `__mutsu_scheduled_transform_apply`)
   whose Rust implementation is exactly what `handle_supply_transform_emit`
   already does (call the mapper per `mode`, then `supplier_emit` into
   `downstream_supplier_id`) — reuse that function directly, do not
   reimplement the mode logic.
3. In `make_live_transform_supply` (and the `.flat` arm), branch on whether
   `attributes` contains `"scheduler"` (and not `"scheduler_interval"`):
   - unscheduled (today's behavior, unchanged): `register_supplier_transform_tap`
     as now — zero behavior/perf change for the common case.
   - scheduled: build the `__ScheduledTransformApply` shim sub (same
     synthesized-`SubData`-calls-one-`MethodCall` idiom as
     `build_scheduled_pump_shim`), pass it through
     `self.wrap_scheduled_callbacks(attributes, shim, None, None, 0.0)`, and
     register the *wrapped* result via the ordinary `register_supplier_tap`
     on the source's `supplier_id` instead of `register_supplier_transform_tap`.
4. Handle `Tap.close`/pump cleanup the same way the `whenever`-in-supply fix
   (ADR-0028 Slice 2, already landed) does: track the returned pump id and
   thread it onto whatever handle owns the downstream Supply's lifetime.
5. Decide `done`/`quit` propagation into the downstream supplier — today
   `TransformCall`'s `done`/`quit` presumably reach the downstream supplier
   through a separate mechanism (need to trace `get_transform_output_supplier_ids`
   consumers); the new path must preserve that.

This is a real, scoped design (not a shortcut), but it is a new subsystem
(shim class + registry-free pass-through since `wrap_scheduled_callbacks`
already returns everything needed) touching 4 files, and needs its own test
matrix (map/grep/do × scheduled/unscheduled × close-cascade) before landing —
hence `todo/deep/` rather than folding into the Slice-2 PR that fixed the
lower-risk categories 1 and the `whenever`-in-supply chokepoint bypass.

## Repro

Save as `tmp/schedule-on-map-deadlock.raku` and run with
`timeout 15 target/debug/mutsu tmp/schedule-on-map-deadlock.raku` — prints
`inner-status: Planned` (mutsu) vs `inner-status: Kept` (raku):

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
say "inner-status: $inner-status";
```
