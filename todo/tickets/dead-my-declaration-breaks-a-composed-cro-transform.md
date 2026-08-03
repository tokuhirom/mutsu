# `supply whenever f(...)` loses its subscription when `f` has a local

`Cro::HTTP::Middleware::Response`-based middleware never reaches the client. The
middleware runs, appends its header, emits; the role's outer `transformer` supply
receives it and emits it too; a raw socket read proves the server writes a
complete, correct response to the wire — and yet
`await Cro::HTTP::Client.get(...)` in the same process resolves to `Nil`. The
same middleware written directly as a `Cro::Transform` (what
`t/http-middleware.rakutest`'s first subtest does) works.

This is why that file passes its first subtest 4/4 and then hangs.

## Minimal trigger

`Cro::HTTP::Middleware::Response.transformer` reaches its `process` through a
helper (`wrap-response-logging`, `lib/Cro/HTTP/Middleware.rakumod:29`):

```raku
supply whenever wrap-response-logging(self, $pipeline, { self.process($_) }) -> $response { ... }
```

Reproduced with a *local* copy of the role, so no vendored file is involved. Only
the helper's body varies (`tmp/mwvar.sh` builds each variant from `tmp/mw16.p6`;
run them with `bash tmp/crorun.sh`):

| # | body of `sub my-wrap-logging(Any $middleware, Supply $pipeline, &process --> Supply)` | result |
| - | --- | --- |
| L | `process($pipeline)` | works |
| K | `my $x; process($pipeline)` | **broken** |
| M | `my $x = process($pipeline); $x` | **broken** |
| I | `if False { my $x; } else { process($pipeline) }` | **broken** |
| H | `if False { my $zzqqx77 = 1; } else { process($pipeline) }` | **broken** |
| G | `if False { process($pipeline); } else { process($pipeline) }` | works |
| F | `if False { $middleware.WHAT; } else { process($pipeline) }` | works |
| A | `if False { } else { process($pipeline) }` | works |

**K vs L is the whole bug: one `my $x;` in the callee.** It needs no
initializer, the name is irrelevant (H uses a name nothing else could collide
with), and it need not even execute (I) — so the damage is not a runtime write
but something about the callee having a local at all. F/G show an ordinary
statement or a call in the same position is harmless.

## The subscription, not the value

Binding the call's result before the `whenever` **fixes it** (`tmp/mwv-K2.p6`):

```raku
my $s = my-wrap-logging(self, $pipeline, { self.process($_) });
supply whenever $s -> $response { ... }      # works
```

So the callee returns the right Supply; what breaks is `supply whenever` applied
directly to the *call expression* when that callee has locals. That is the thing
to fix — and the `my $s` form is the workaround to confirm any candidate fix
against.

## What does NOT reproduce it

All green on mutsu and raku, so the Cro `Cro::HTTP::Server` pipeline is still a
required ingredient — do not keep shrinking, grow these toward the server:

- `tmp/whenever-call.p6` — exactly the `supply whenever wrap(...)` shape with a
  `my $unused;` in `wrap`, tapped directly. Passes.
- `tmp/deadmy2.p6` — the full role / `does Cro::Transform` shape, tapped directly.
- `tmp/deadmy4.p6` — the same role composed via `Cro.compose(Inc, Doubler)` over
  a `Cro::Message`, then tapped. So plain `Cro.compose` is not enough either.

The failing reproducer is `tmp/mwv-K.p6`; `tmp/mwv-noafter.p6` is the control
(same file, middleware not installed — passes, so the helper alone is harmless).
`tmp/mwraw2.p6` is the raw-socket read that proves the server side is correct.

## Where to look

Between `deadmy4.p6` (passes) and `mwv-K.p6` (fails) what is left is the
`Cro::HTTP::Server` pipeline: the connection manager, the sink end, the
`before`/`after` insertion around the application, and the fact that the
middleware runs on the worker thread handling the socket. Add those to
`deadmy4.p6` one at a time. Once it reproduces standalone, the question is how
the `whenever` operand's Supply is captured when the operand is a call whose
frame has locals — compare the K and L variants' bytecode for the `supply
whenever` operand (`--dump-bytecode`).

## Blast radius

`http-middleware`, `http-session-inmemory`, `http-session-persistent`,
`router-auth`, `http-auth-basic*` and `http-log-file` in the vendored Cro::HTTP
suite all use `Cro::HTTP::Middleware::*` roles and should move together with this.
