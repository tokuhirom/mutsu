# `Promise(supply { ... })` ignores a `whenever` registered from inside another whenever's body

This is the remaining structural blocker for the Cro::HTTP **client**.

**The `react` half of this is fixed** (see
`news/2026-08/react-adopts-nested-whenever.md`): `run_react_event_loop`'s drive
loop adopts subscriptions a running `whenever` body registers. The
`Promise`/`await` half below still builds its set once and resolves as soon as
that set drains. Every
`Cro::HTTP::Client` request is one of these:

```raku
Promise(supply {
    sub do-request-on-pipeline() {
        whenever self!get-pipeline(...) -> $pipeline {          # outer
            whenever $pipeline.send-request($request-object) {  # inner, registered
                ...                                            # from the outer body
                emit $response;
            }
        }
    }
    ...
})
```

mutsu drives `Promise(<on-demand supply>)` synchronously in
`Interpreter::supply_promise_on_demand` (`src/runtime/supply_promise.rs`): it
runs the supply body once with `run_on_demand_body`, splits what the body
emitted into "subscription markers" and plain values, turns each marker into a
`ReactSubscription`, and hands the set to `drive_react_subscriptions` under
`SupplyDrivePolicy::Promise`.

The subscription set is computed **once, before the loop starts**. A `whenever`
that a running whenever *body* registers never joins it. So in the Cro case the
outer `get-pipeline` promise fires, its body runs (and registers the inner
`send-request` whenever, which goes nowhere), the outer subscription reports
Done, the driver sees no live subscriptions left, and keeps the promise with
the last emitted value — `Any`. `await` then hands the caller an `Any` where a
`Cro::HTTP::Response` belongs, and the request silently "succeeds" with no
response:

```
No such method 'status' for invocant of type 'Any'
```

Minimal repro (mutsu hangs here rather than answering `Any`, but it is the same
missing registration — the inner subscription is not part of the driven set, so
nothing can ever complete it):

```raku
my $outer = Promise.new;
my $inner = Promise.new;
my $s = supply {
    whenever $outer -> $a {
        whenever $inner -> $b { emit "$a/$b" }
    }
};
my $p = Promise($s);
start { sleep 0.2; $outer.keep(1); sleep 0.4; $inner.keep(2) }
say await $p;      # raku: "1/2"   mutsu: hangs to the 30s deadline, then Nil
```

## Why it is large

- The subscription set has to become **mutable while the loop runs**: a body
  that calls `whenever` must be able to append to the set the driver is
  iterating, and the new source's waker must be registered on the live
  `ReactWaker`. `drive_react_subscriptions_loop` takes `&mut [ReactSubscription]`
  (a slice — cannot grow) and registers wakers once, up front, in
  `drive_react_subscriptions_inner`.
- The "supply is done when all its whenevers have completed" rule has to hold
  over that growing set, not over the initial one. Today an empty set means
  "keep with the last value", which is what silently produced `Any`.
- The same registration path is shared by `react`, `supply`-block taps and this
  `Promise` coercion (three `SupplyDrivePolicy` variants), so the fix has to be
  made in the shared driver rather than in the Promise policy alone.
- `Promise(<supply>)` is driven **synchronously on the calling thread** with a
  30-second deadline. Raku returns a `Promise` immediately and resolves it from
  the supply's own scheduling. Cro calls this inside `method request`, so mutsu
  blocks the requester for the whole exchange. Fixing the nesting without also
  fixing this leaves a semantic difference that will bite concurrent clients.

## Affected

- Every `Cro::HTTP::Client` request: `t/http-auth-basic.rakutest`,
  `t/http-auth-basic-with-session.rakutest`, `t/http-session-inmemory.rakutest`,
  `t/http-session-persistent.rakutest`, `t/http-middleware.rakutest`,
  `t/router-auth.rakutest`, and the round-trip half of `t/http-router.rakutest`
  in the vendored Cro::HTTP suite.
- Any Raku code that builds a request/response exchange as nested `whenever`s
  under a `Promise`-coerced supply.

## Entry points

- `src/runtime/supply_promise.rs` — `supply_promise_on_demand`, which builds the
  subscription set.
- `src/vm/vm_react_subscriptions.rs` — `drive_react_subscriptions_inner` (waker
  registration) and `drive_react_subscriptions_loop` (the poll loop and the
  "all subscriptions done" decision).
- `self.current_react_waker` already exists so that "sources wired up mid-loop"
  can wake the loop; it is the seed of the mechanism this needs, but nothing
  currently *adds a subscription* through it.
