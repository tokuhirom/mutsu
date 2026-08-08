# `Supply.do` callbacks are silently skipped for values delivered through the async live-tap path

## Summary

`Supply.do($cb)` on an on-demand (`supply { ... }`) source only runs `$cb` for
values the source's body emits **synchronously** during its initial
(re-)execution. Any value the same source delivers **asynchronously** later —
via the live-tap mechanism that drives a nested `whenever` chain — reaches the
final subscriber untouched, `$cb` never having run.

This is a follow-on to the fix in `src/runtime/native_supply_dispatch.rs`'s
`"do"` arm (see `news/2026-08/supply-do-on-demand-source.md` /
`t/supply-do-on-demand-source.t`), which fixed the simpler case: a `.do()`
result derived from an on-demand source used to be a permanent dead end
because the fallback branch dropped `on_demand_callback` entirely, copying
only (empty) `values`. That fix makes `.do` work correctly for a source whose
body emits its values with plain synchronous `emit` calls. It does **not**
fix sources whose body's real work happens inside a nested `whenever` —
which is the common case for any non-trivial `supply { ... }` block, and
specifically the case that still leaves the vendored Cro suite's
`http-auth-basic.rakutest` at 3/5 passing subtests (previously 0/5 — a
permanent hang; see below).

## Root cause (traced with `MUTSU_DEBUG_DO`-gated `eprintln!`, since removed)

In `src/runtime/native_supply_mut_methods.rs`, `native_supply_mut`'s `"tap"`
handling for an on-demand source (`attrs.get("on_demand_callback")`, starting
around line 280) collects two disjoint kinds of results from
`run_on_demand_body`:

1. **`plain_values`** — values the body emitted synchronously during this one
   call. These feed the `do_cbs` loop near line 991 (`for cb in cbs { ...
   call_sub_value(cb, [v]) }`) — `do_callbacks` DOES apply here.
2. **Live subscriptions** — `whenever <source> { ... }` markers found in the
   body's output. For a chained on-demand inner source (the branch around
   line 601-671, `inner_attrs.as_map().contains_key("on_demand_callback")`),
   the code registers the OUTER tap callback (`tap_cb`, the real downstream
   subscriber — NOT wrapped with `do_cbs`) directly onto the emitter via
   `register_supplier_tap(emitter_supplier_id, tap_cb.clone(), delay_seconds)`
   (around line 437-449, `outer_tap_registered`). Every later async
   `$emitter.emit(val)` dispatches straight to `tap_cb`, bypassing `do_cbs`
   entirely.

Confirmed empirically: for Cro's `Cro::HTTP::Auth::Basic.process-responses`
(`$responses.do: -> $response { ... }`), the on-demand tap-entry for the
`.do`-derived Supply logged `do_cbs.len()=1` (our fix correctly propagated the
callback) but `values.len()=0` at the same call — the body's real 401
response is delivered later through the registered live tap on
`emitter_supplier_id`, where `do_cbs` is never consulted.

## Why it matters

Any middleware or pipeline stage that does `$supply.do($cb)` where `$supply`
is backed by a `whenever`-driven chain (essentially all real Cro middleware,
and any non-toy `supply { ... }` block) silently drops the side effect for
async-delivered values. In the Cro case this means
`Cro::HTTP::Auth::Basic.process-responses`'s `WWW-Authenticate` header never
gets added to a 401 response — `t/http-auth-basic.rakutest` subtests 2 and 4
still fail (`.response matches` — the header is absent) even though the
request/response round-trip itself now completes.

## Repro

```
$ mutsu -e '
my $inner = supply { whenever Promise.in(0).then({ 1 }) -> $v { emit $v } };
my @seen;
$inner.do({ @seen.push($_) }).tap({ say "tap $_" });
sleep 0.2;
say "seen: @seen[]";
'
```
Expected (raku): `tap 1` then `seen: 1`. mutsu: `tap 1` then `seen: ` (empty —
the do callback never ran for the asynchronously-emitted value).

## Suggested direction

When registering the live tap on `emitter_supplier_id` for a source that also
carries `do_callbacks` (or more generally, whenever `attrs` has
`do_callbacks` and the delivery path is the live/async one, not the
`plain_values` loop), wrap the registered callback so it runs the
`do_callbacks` chain against the value before forwarding to the real `tap_cb`.
This likely needs a small synthetic-callable helper (a Rust closure exposed as
a callable `Value`) since `register_supplier_tap` stores whatever `Value` it's
given and later invokes it via `call_sub_value` — check whether such a
"native wrapper callable" pattern already exists elsewhere in the supply
machinery (e.g. `TransformMode`/`register_supplier_transform_tap` for
`make_live_transform_supply`, which is exactly this pattern for a source that
already has a stable top-level `supplier_id` — the on-demand case lacks that
stable id up front, which is why `make_live_transform_supply` bails via
`supplier_id_from_attrs` returning `None` for it).

## Affected files

- `src/runtime/native_supply_mut_methods.rs` (~line 280-460, ~line 601-700,
  ~line 960-998)
- `src/runtime/native_supply_dispatch.rs` (the `"do"` arm, already partially
  fixed)
