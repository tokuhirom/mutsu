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

Even without nesting the completion is wrong, which is the first thing to fix:

```raku
my $a = Promise.new;
my $p = Promise(supply { whenever $a -> $v { emit "got $v" } });
start { sleep 0.2; $a.keep(1) }
say await $p;      # raku: "got 1" at once   mutsu: Nil after 30 seconds
```

## What is left, and what was tried

The **growable subscription set is already in place** (the `react` fix): the
shared drive loop adopts `pending_react_subscriptions` each round and registers
the new sources' wakers. What the `Promise` policy still gets wrong is
*completion*, and it is a distinct problem from the nesting:

1. **`Promise(supply { whenever $p { emit … } })` never completes early.** Even
   the un-nested case sits until the 30-second deadline and is then kept with
   `Nil`. Raku's rule is "kept when the Supply is done, with the final value",
   and a `supply` block is done once all its `whenever`s have completed — so the
   loop should keep the promise the moment every subscription is done.
   The deadline path (`promise.keep(Value::NIL, …)`) should use the last value
   too.
2. **The emitted value does not reach `last_value`.** An `emit` inside a
   `supply` body is rewritten to `$emitter.emit(…)` and goes to the *supplier
   registry* keyed by the `emitter_supplier_id` that `supply_promise_on_demand`
   allocated — not to the `supply_emit_buffer` frame the Promise policy pushes
   around each callback. So "keep with the last emitted value" has to read the
   emitter supplier, not that frame. A naive `all_done → keep(last_value)` was
   tried and still answered `Nil` for this reason; do not repeat it without
   fixing the value source first.
3. **A `whenever` marker registered by a nested body lands in that same
   `supply_emit_buffer` frame**, where the Promise policy currently treats
   `emitted.last()` as an emitted value. Markers must be split out (they are the
   4-element `[source, body, [LAST…], [QUIT…]]` arrays) and handed to
   `pending_react_subscriptions` instead.
4. **`Promise(<supply>)` is driven synchronously on the calling thread** with a
   30-second deadline. Raku returns a `Promise` immediately and resolves it from
   the supply's own scheduling. Cro calls this inside `method request`, so mutsu
   blocks the requester for the whole exchange. Fixing completion without also
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
