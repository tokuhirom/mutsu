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

## The remaining bug: `RouteHandler.invoke` never emits its response

Routing is now entirely correct. `tmp/bm3.p6` (`route { before-matched
LowerCase; delegate <*> => $application }`) reaches the delegated route set's
own handler, matching raku step for step — and then stops:

```
[DBG-T] whenever fired, self=RouteSet|1169 requests=Supply|2499   <- inner, correct
[DBG] routing-outcome for /index.shtml = (0, \("index.shtml"))
[DBG] handler 0 = Cro::HTTP::Router::RouteSet::RouteHandler args=\("index.shtml")
                                       <- raku prints `handler emitted Cro::HTTP::Response` here
```

So the next thing to dig into is `RouteHandler.invoke`
(`lib/Cro/HTTP/Router.rakumod`, around line 205-264), specifically the
`@!before-matched`-taken branch:

```raku
my $current = supply emit $request;
$current = self!append-middleware($current, @!before-matched, %connection-state);
my $response = supply whenever $current -> $req {
    whenever self!invoke-internal($req, $args) { emit $_; }
}
return self!append-middleware($response, @!after-matched, %connection-state);
```

The `whenever` nested inside a `whenever` body — registered while the drive loop
is already running, so it goes onto `pending_react_subscriptions` rather than a
registration frame (`runtime/subtest.rs`, `run_whenever_with_value`) — is the
prime suspect, together with `!invoke-internal`'s `start { … }` block. Note the
handler that *does* work (`include`, and `delegate` with no middleware) takes
the `else` branch, `return self!invoke-internal($request, $args)`, which has no
nested `whenever` at all — that asymmetry is the lead.

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
