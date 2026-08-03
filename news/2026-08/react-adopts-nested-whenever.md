# A `whenever` inside a `whenever` body now joins the react

The react drive loop built its subscription set **once**, from the markers the
react body registered, and then iterated a fixed slice of them. A `whenever`
written inside another `whenever`'s body only registers when that body *runs* —
which is inside the loop, long after the set was built — so it was dropped on
the floor:

```raku
my $outer = Promise.new;
my $s = Supplier.new;
start { sleep 0.2; $outer.keep(1); sleep 0.2; $s.emit(42); $s.done }
react {
    whenever $outer -> $v {
        say "outer $v";
        whenever $s.Supply -> $x { say "INNER $x" }   # never fired
    }
}
```

mutsu printed `outer 1` and then ended the react, because the outer
subscription was the only one it knew about and it had completed. Raku prints
`INNER 42` and keeps the react alive until the nested subscription finishes
too.

This is the shape every socket server is written in — accept a connection in
one `whenever`, read it in a nested one — so it became load-bearing the moment
a loopback connection stopped being an in-memory socket pair
(`news/2026-08/loopback-connect-is-a-real-tcp-connection.md`) and started
delivering its bytes asynchronously.

The loop now takes a growable subscription list and adopts, at the top of every
round, whatever a running body registered: it builds the new
`ReactSubscription`s with the same code the initial batch uses (extracted as
`build_react_subscriptions`), registers their supplier sinks and promise /
channel / receiver wakers on the live waker, and keeps driving until they are
done too. `whenever` itself notices it is inside a running drive loop
(`react_active > 0`) and queues its marker on `pending_react_subscriptions`
rather than taking the "not in a react" direct-tap path.

## The subtlety: a nested callback owns its lexicals

`call_react_callback` deliberately drops a callback's per-instance closure
state before each call, so that sibling `whenever`s of the react body all read
the *live* shared lexicals instead of restoring stale private snapshots of
them. For a nested `whenever` that is exactly wrong: it closes over the
enclosing `whenever` body's frame, which has already exited by the time values
arrive, so the per-instance state is the only copy of those lexicals. Dropping
it reset them on every value — HTTP::UserAgent's `TestServer` accumulates a
request with `my Buf $in-buf` in the accept body and appends to it from the
read body, and with the state cleared every chunk looked like the first one, so
the server never saw a complete request and the whole
`230-binary-request.rakutest` suite hung. Adopted callbacks are recorded in
`nested_react_callbacks` and keep their state.

Pin: `t/react-nested-whenever.t` (five shapes, including two levels of nesting,
`done` from a nested body, and a `Channel` source), plus the roast tests this
unblocked: `S17-promise/nonblocking-await.t`'s "Got 20 responses from async
socket server that does non-blocking await" and the second half of
`S32-io/IO-Socket-Async.t`.

The `Promise(supply { ... })` coercion has the same gap and is *not* fixed
here: it builds its subscription set through `supply_promise_on_demand` and
resolves as soon as that set drains, so a nested `whenever` still leaves it
answering `Any`. That is what still blocks the `Cro::HTTP::Client` response
path — see
`todo/deep/promise-of-an-on-demand-supply-ignores-nested-whenever.md`.
