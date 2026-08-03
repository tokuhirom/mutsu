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
  `t/supply-block-reinstantiated.t`). This removed the infinite
  `[lc] target=...` loop.

## The remaining bug

`Cro::HTTP::Router::RouteSet.transformer` is

```raku
method transformer(Supply:D $requests) {
    supply {
        whenever $requests -> $request { ... @!handlers ... }
    }
}
```

A `delegate` puts **two live instances** of this one parse site in the pipeline:
the outer route set (which owns the `DelegateHandler`) and the delegated inner
route set. Instrumenting the body shows the second dispatch running with the
*inner* invocant but the *outer* `$requests`:

```
[DBG-T] body entered,   self=RouteSet|1169 requests=Supply|2499   <- inner, correct
  [lc] got /index.SHTML
[DBG-T] whenever fired, self=RouteSet|1169 requests=Supply|2103   <- outer's $requests
```

`self` is right because `resolution_call_sub.rs` force-installs a closure's
captured `self` (it is lexical in Raku). `$requests` is wrong because it is an
ordinary free variable of the supply block — a parameter of the enclosing
`transformer` frame, which has already returned — and the `merge_all`
caller-priority merge in `resolution_call_sub.rs` lets the *calling* frame's
same-named binding win for anything that is not in `authoritative_free_vars` /
`authoritative_captures`.

`exec_whenever_scope_op` (`src/vm/vm_scope_ops.rs`) already builds an
`owned_lexicals` list for exactly this reason, but it covers only the supply
body's `my` declarations plus (now) its emitter. A supply block body is a scope
its caller never re-enters and whose `whenever` callbacks are dispatched later
from arbitrary frames and threads, so **its free variables should be owned too**
— that is the shape of the fix to try first:

```rust
owned.extend(code.free_var_syms.iter().copied());
```

Captures that genuinely must stay live are `ContainerRef` cells, and those are
force-installed ahead of the authoritative check (`resolution_call_sub.rs:411`),
so owning free vars by name should not freeze a shared cell. This needs a real
`make test` + roast run: it widens the authoritative set for every supply block
in the codebase.

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
