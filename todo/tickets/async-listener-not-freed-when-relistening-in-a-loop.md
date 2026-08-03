# A third server bound to the same port returns empty bodies

## Fixed part (do not re-derive)

The loop-body `my $tap` that reverted to the previous iteration's `Tap` once a
connection was accepted is **fixed** — see
`news/2026-08/decl-in-flight-spawn-must-not-unmask.md`. Root cause: the `.tap`
in the *initializer* spawned a worker while the declaration was still in flight,
so `clone_for_thread` seeded the shadowed outer value into the shared lane and
dropped the re-declaration mask; the next `await`'s `sync_shared_vars_to_env`
pulled it back over the slot. `thread_decl_in_flight` now keeps the mask for
that window. Pinned by `t/io-socket-async-relisten-loop.t`.

The earlier `IO::Socket::Async.listen` finding is also fixed:
`TcpListener::bind(&str)` walked *every* address the host resolved to and took
the first that succeeded, so a re-listen whose previous listener was still bound
to `[::1]` quietly bound `127.0.0.1` instead — two listeners for one
`localhost:port` (two LISTEN rows in `ss -tan`), and a client reached whichever
its own resolution picked. `listen` now resolves once and binds that single
address.

## What still fails

Re-binding one port across *more than two* rounds still breaks, and it is no
longer a stale-`$tap` problem: each round now observes its own `Tap` and the
port is still bindable afterwards (that is exactly what the pin test asserts).
What fails is the **serving**, from round 2 on.

`tmp/mw6.p6` (six sequential `Cro::HTTP::Server`s on one port, each `.start`ed,
requested and `.stop`ped) prints:

```
round 2: OK
round 2: OK
round 2:          <- empty body from here on
round 2:
round 2:
round 2:
```

Two things are wrong and they may or may not be the same bug:

1. **The body is empty from the third round on.** The first two servers answer
   `OK`; every later one returns an empty body.
2. **`$i` reads `2` in every iteration.** `say "round $i: $body"` prints `round
   2` even in the first iteration — the `for ^6 -> $i` parameter, not a `my`.
   This is NOT reproducible in isolation: `for ^4 -> $i { my $r = await start {
   $i * 10 }; say "round $i: $r" }` is correct on mutsu and raku
   (`tmp/loopvar.p6`), as is the socket-listen form. So it needs the Cro stack
   to show up, and it is worth isolating before chasing (2) as its own bug —
   it may be a symptom of (1) rather than a second defect.

`t/http-middleware.rakutest` shows the same shape: its first subtest now passes
4/4 (it used to hang there), the second subtest's four assertions all fail with
an empty body, and the file then hangs — so a `timeout` is still needed when
running it.

The other multi-server files in the vendored Cro::HTTP suite —
`http-auth-basic*`, `http-session-*`, `router-auth`, `http-log-file` — are the
same family and are expected to move together with this.

## Where to look next

The listener side is now sound, so start from the *connection* side: what the
third `Cro::HTTP::Server` on a port does differently from the first two.
`tmp/mw6.p6` is the smallest reproducer; `bash tmp/crorun.sh tmp/mw6.p6` runs it
against the staged dists in `tmp/cro-work/` (it needs the release binary).
