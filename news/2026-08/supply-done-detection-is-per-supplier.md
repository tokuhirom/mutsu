# "Did this supply block call `done`?" is now asked per supplier, not per process

A Cro HTTP server and a Cro HTTP client sharing one mutsu process lost roughly a
quarter of their round trips. The client wrote the request, the server read the
bytes off the socket, and the request then vanished between
`Cro::TCP::ServerConnection.incoming` and `Cro::HTTP::RequestParser`'s
`whenever`. The client's response promise was eventually kept with `Nil` by the
react drive loop's 30-second deadline, so `await`ing it answered `Any` — which
is why the symptom usually surfaced as an unrelated-looking coercion error such
as "Impossible coercion from 'Any' into 'Promise'".

## Root cause

Tapping a `supply { … }` block runs its body once and then asks whether the body
completed the supply by calling `done`. `run_on_demand_body`
(`src/runtime/supply_promise.rs`) answered that question by snapshotting a
**process-global** monotonic counter of `Supplier.done` calls before running the
body and comparing it afterwards:

```rust
let done_before = supplier_done_count();
let result = self.call_sub_value(on_demand_cb, vec![emitter], false);
let body_ran_done = supplier_done_count() > done_before;
```

The counter ticks for a `done` on *any* supplier, from *any* thread. With a
server and a client in one process there are always several live pipelines
completing suppliers concurrently, so any `done` that happened to land inside
that window made `body_ran_done` spuriously true. `native_supply_mut`'s tap path
then took its `if body_done` branch and invoked the completion marker
immediately, which cascades through `close_upstream_taps` and closes every tap
the same tap cascade had *just* registered — including the connection's
`incoming` tap. The next `emit` on that supplier found `closed: true` on its only
tap, produced zero actions, and the request was silently dropped.

`run_whenever_quit_phaser` ("did this QUIT phaser call `done`?") read the same
global counter and had the same false-positive.

## Fix

Count `done` calls per supplier and per thread instead of per process:

- `bump_supplier_done_count(Option<u64>)` now records the call both in a
  per-supplier map and in a thread-local total.
- `run_on_demand_body` compares the count **on its own emitter** when it has an
  emitter supplier id, and falls back to this thread's total when it does not
  (the body runs synchronously on the calling thread, so another thread's
  pipeline is never its doing).
- `run_whenever_quit_phaser` uses the thread-local total.

## Result

The reproduction (one Cro server plus twenty `Cro::HTTP::Client.get` calls in a
single process) goes from 4-6 lost round trips to 0/20, and drops the 30-second
stall each loss used to cost. Cro's `t/http-middleware.rakutest` was a coin
flip before — 1, 3, 6 and 9 failures across four consecutive runs of the same
binary — and is now deterministic at 10 of 11 subtests passing on every run.
The one remaining failure (subtest 4,
`Cro::HTTP::Middleware::RequestResponse`) is a separate issue, as is the
`No matching candidates for proto sub: before-matched` error the file hits after
its last subtest.

Pinned by `t/supply-done-detection-is-per-supplier.t`, which taps a two-stage
on-demand pipeline forty times while a background thread does nothing but
create, tap and complete unrelated suppliers. Before the fix 36 of the 40 rounds
lost their values; after it, none do.
