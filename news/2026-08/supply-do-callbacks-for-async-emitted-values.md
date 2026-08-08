# `Supply.do` callbacks now also fire for asynchronously-delivered values

A companion fix to `news/2026-08/supply-do-on-demand-source.md`: that fix made
`Supply.do($cb)` on an on-demand (`supply { ... }`) source work at all instead
of being a permanent dead end, but only for values the source's body `emit`s
*synchronously*. It left `t/http-auth-basic.rakutest` at 3/5 passing — the
request/response round-trip completed, but the 401 response was still missing
its `WWW-Authenticate` header.

## Root cause

`native_supply_mut_methods.rs`'s on-demand tap handling separates what a
source's body produces into two disjoint kinds: values it `emit`s
synchronously during the current call (`plain_values`, which do run through
the `do_callbacks` loop), and `whenever <source> { ... }` subscriptions, which
register the real downstream subscriber directly on the source's emitter via
`register_supplier_tap(emitter_supplier_id, tap_cb.clone(), ...)` — bypassing
`do_callbacks` entirely. Any value delivered later through that live
registration (essentially every non-trivial `supply` block, whose real work
happens inside a nested `whenever` rather than a bare `emit`) reached the
final subscriber with `$cb` never having run.

Confirmed with `Cro::HTTP::Auth::Basic.process-responses`
(`$responses.do: -> $response { ... }`, adding the header when
`$response.status == 401`): instrumented tracing showed the on-demand tap
entry saw `do_callbacks.len()=1` (the earlier fix correctly propagated the
callback) but `plain_values.len()=0` at the same call — the 401 response is
always delivered through the async path.

```
$ mutsu -e '
my $inner = supply { whenever Promise.in(0).then({ 1 }) -> $v { emit $v } };
my @seen;
$inner.do({ @seen.push($_) }).tap({ say "tap $_" });
sleep 0.3;
say "seen: @seen[]";
'
# before: tap 1 / seen:        (do callback never ran)
# after:  tap 1 / seen: 1
```

## Fix

Wrap the real subscriber callback with the source's `do_callbacks` chain
before registering it on the emitter (`register_outer_tap_with_do_callbacks`,
`native_supply_methods.rs`), using a small marker Value
(`__SupplyDoWrappedTap`, the same pattern `__SupplyDoneChain` already uses for
bundling multiple done callbacks into one slot) that `call_supply_tap` — the
single function through which every registered supplier tap is ultimately
invoked — unwraps before dispatching: run each `do_callbacks` entry against
the value, then forward to the real subscriber.

Pin: `t/supply-do-on-demand-async-emit.t`. `http-auth-basic.rakutest` and
`http-auth-basic-with-session.rakutest` are now 5/5.
