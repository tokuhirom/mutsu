# `Supply.do` on an on-demand source was a permanent dead end

`t/http-auth-basic.rakutest` and `t/http-auth-basic-with-session.rakutest` in
the vendored Cro suite, plus `t/http-router.rakutest`, hung forever (`rc=124`,
no TAP output at all — not even the plan line). `perf`-sampled stack traces of
the hung process showed one thread parked forever in
`SharedPromise::wait` (called from `build_react_subscriptions`, waiting on a
client request that never got a response) while a socket-listener thread idled
normally — the server had bound its port fine, but no HTTP response ever left
it.

## Root cause

`Supply.do($cb)` builds a derived Supply that calls `$cb` as a side effect and
passes each value through unchanged. `native_supply_dispatch.rs`'s `"do"` arm
tries a live (Supplier-backed) fast path first
(`make_live_transform_supply`), and otherwise falls back to copying the
source's `values` and `live` attributes into the new Supply. That fallback
assumed there was something in `values` to copy — but an on-demand source
(`supply { ... }`, the common case for any real `supply` block) has no
materialized `values` at all; it only has an `on_demand_callback` that runs
its body on first tap. The fallback copied the (always-empty) `values` and
silently dropped `on_demand_callback`, so the resulting Supply had no source
whatsoever. Nothing ever tapped it produced anything, forever:

```
$ mutsu -e 'my $s = supply { emit 1; emit 2 };
           $s.do({ say "do-cb $_" }).tap({ say "tap $_" });'   # printed nothing
```

Cro's `Cro::HTTP::Auth::Basic.process-responses` is exactly this pattern
(`$responses.do: -> $response { ... }` on an on-demand response pipeline), and
it sits in the middle of every response leaving the server when `before =>`
middleware is configured — so the whole response pipeline dead-ended and the
client waited forever.

## Fix

When the fallback's source has `on_demand_callback` instead of `values`,
carry the callback forward instead of the (empty) `values`. A later
`.tap()`/`whenever` on the result re-runs the original on-demand body and
applies the accumulated `do_callbacks` chain generically — that machinery
already existed and needed no changes; it just never received the callback
before.

Pin: `t/supply-do-on-demand-source.t`.

## Effect on the Cro::HTTP suite

`http-auth-basic.rakutest` and `http-auth-basic-with-session.rakutest` no
longer hang: `http-auth-basic` now completes all five subtests structurally
(3/5 passing). The remaining two failures are a distinct, deeper bug — `.do`
callbacks are still skipped for values an on-demand source delivers
*asynchronously* through a nested `whenever` (as opposed to a synchronous
`emit` in the body), which is what actually adds the `WWW-Authenticate`
header to Cro's auto-generated 401 response. Filed as
`todo/deep/supply-do-callbacks-not-applied-to-async-emitted-values.md`.
