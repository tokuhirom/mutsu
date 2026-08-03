# A supply block's free variables leak to the caller when its `whenever` fires

`Cro::HTTP::Middleware::*`-based middleware still does not reach the client.
With `before-matched LowerCase` installed in a route block, the middleware runs
once, lowercases the target, emits — and the request then re-enters the route
matcher with the *wrong* route set state, so the client's
`await Cro::HTTP::Client.get(...)` resolves to `Nil`.

## What has already been fixed (do not re-investigate)

The original form of this ticket blamed `supply whenever f(...)` losing its
subscription when `f` had a local. That framing is **obsolete** — three separate
bugs behind it have been fixed:

- `Parameter.constraints` now returns an `all()` junction, so Cro's route
  compiler stops adding a bogus signature bind check (see
  `news/2026-08/cro-delegate-route-block.md`).
- `supply STATEMENT` parses, so `DelegateHandler.invoke`'s
  `my $current = supply emit $req;` no longer dies on a supply worker (same
  news entry). This alone made `route { delegate <*> => $inner }` work.
- A supply block's generated emitter is now owned by the `whenever` closures it
  creates, so two live instances of one parse site no longer feed each other
  (`news/2026-08/supply-block-reinstantiated.md`, pinned by
  `t/supply-block-reinstantiated.t`).
- A supply block's compiler-vouched captures are owned too, so the inner route
  set's callback stops reading the outer's `$requests`
  (`news/2026-08/supply-block-captured-lexical.md`, pinned by
  `t/supply-block-captured-lexical.t`).

## The remaining bug: the outer route set ping-pongs with the body serializer

Routing is now **entirely correct**, matching raku step for step: `tmp/dg1.p6`
(`route { delegate <*> => $inner }`) reaches the delegated route set's own
`RouteHandler`, which runs and produces a 200. The response then loops forever
between two stages of the delegate pipeline:

```
[DBG-T] body entered,   self=RouteSet|1219                 <- outer route set
[DBG-D] step3 body-serializers
[DBG-T] body entered,   self=RouteSet|1169                 <- delegated route set
[DBG] handler emitted Cro::HTTP::Response from routeset RouteSet|1169   <- correct, once
[DBG-S] body-serializer stage got Cro::HTTP::Response
[DBG] handler emitted Cro::HTTP::Response from routeset RouteSet|1219   <- outer, forever
[DBG-S] body-serializer stage got Cro::HTTP::Response
[DBG] handler emitted Cro::HTTP::Response from routeset RouteSet|1219
…
```

raku prints exactly two `handler emitted` lines and returns.

The two stages are `DelegateHandler.invoke`'s

```raku
$current = $!transform.transformer($current);        # the delegated RouteSet
$current = self!append-body-serializers($current);   # supply whenever $pipeline { … emit $_ }
```

and the OUTER `RouteSet.transformer`'s

```raku
whenever $handler.invoke($request, $args) -> $response { emit $response }
```

which is legitimately subscribed to the body-serializer stage's output (that is
what `$handler.invoke` returns). Its `emit $response` should go *downstream* to
the connection's response pipeline; instead it lands back in the body-serializer
stage, which re-emits, and round it goes.

That is the same emitter-cross-talk family as #5830 / #5831, one level further
out: an `emit` in a `whenever` body reaching the emitter of the supply block
whose frame happens to be *dispatching* it, rather than its own. Note this
`emit` sits in a `whenever` **nested inside another `whenever` body**, so its
callback is built by `run_whenever_with_value` at dispatch time
(`src/runtime/subtest.rs:391-419`, the `pending_react_subscriptions` path) —
`exec_whenever_scope_op`'s `owned_lexicals` is computed from the *enclosing
supply body's* `CompiledCode`, so check whether the nested registration still
gets one. Reproduce with `CRODBG=1 bash tmp/crorund.sh tmp/dg1.p6` and compare
against `tmp/crorunraku.sh`.

Synthetic versions of this shape do NOT reproduce (`tmp/nest1.p6`,
`tmp/nest2.p6` — nested `whenever <Promise>` two route-set layers deep, both
green). Grow those toward the server rather than shrinking further.

## Reproducers

- `tmp/bm1.p6` — `route { before-matched LowerCase; after-matched STS; delegate
  <*> => $application }`. Fails on mutsu, passes on `raku`.
- `tmp/bm3.p6` — the same with only `before-matched`; the trace above comes from
  this one.
- `tmp/dg1.p6`, `tmp/dg2.p6` (`DGVAR=A..D`) — the plain `delegate` variants,
  which all pass now.
- Run them with `bash tmp/crorund.sh <file>` (debug binary) / `tmp/crorun.sh`
  (release) / `tmp/crorunraku.sh` (raku). `CRODBG=1` turns on the `[DBG…]` notes
  that the staged copy of `Cro/HTTP/Router.rakumod` carries — those edits live
  only in `tmp/cro-work/` and must not be vendored.

## Blast radius

`http-middleware` (subtest 2 onward), `http-session-inmemory`,
`http-session-persistent`, `router-auth`, `http-auth-basic*` and
`http-log-file` in the vendored Cro::HTTP suite all use
`Cro::HTTP::Middleware::*` and should move together with this.
