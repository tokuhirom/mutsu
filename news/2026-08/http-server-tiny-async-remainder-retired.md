# HTTP::Server::Tiny async-serving remainder ticket retired

`todo/tickets/http-server-tiny-async-serving-remainder.md` (filed 2026-08-02) tracked three
concrete gaps in the worker-thread-vs-`react`-control-frame interaction that
`IO::Socket::Async`-based servers rely on:

1. Keep-alive: a connection's `Supply` emitting more than once (multiple requests on one
   connection).
2. Chunked request bodies (multiple `whenever $conn.Supply(:bin)` emits assembled by the
   consumer).
3. `done`/`last` raised from inside a `whenever` tap callback (which runs on a worker thread)
   correctly terminating the enclosing `react` block.

All three were re-verified against `raku` with targeted repros (`IO::Socket::Async.listen` +
a real client connection, not a synthetic `Supply`):

- A `done` called from a `Promise.in`-driven tap already matched `raku` (this was already known
  to work).
- A nested `whenever $listener -> $conn { whenever $conn.Supply(:bin) { ...; done; } }` driven by
  an actual `curl` request: mutsu now matches `raku` exactly — the inner `done` terminates the
  whole outer `react` block, `server done` prints, and the client receives the response.
- A keep-alive-style server that lets `$conn.Supply(:bin)` emit twice before closing, exercised by
  a real two-message client: mutsu matches `raku` output line-for-line.

All three were already fixed as a side effect of the wider whenever/react/tap campaign that
shipped for Cro (ADR-0008 push-based `Supply` event delivery, the schedule-on tap-deferral fix,
and related work through 2026-08-13) — no code change was needed this session, only
verification.

The ticket's two related "separate" blockers are already tracked in their own homes and were
never specific to `HTTP::Server::Tiny`:

- **B1** (typed-parameter `var_type_constraint` leakage) — folded into
  `todo/deep/bare-name-type-constraint-store-is-scope-blind.md`, which is actively being worked
  (routine-scoped and block-scoped scalars are fixed; containers, `if`/`while` block bodies, and
  `for`-loop typed params remain open there).
- **B2** (a detached `start { react { whenever $chan { } } }` needing an await to be driven) —
  superseded by [ADR-0020](../../docs/adr/0020-shared-worker-pool.md)
  (shared worker pool / elastic growth / blocking await), proposed 2026-08-05.

`HTTP::Server::Tiny` itself is not vendored or bundled (Cro is the web-framework battery target,
per `docs/batteries/cro-http.md`), so there is no remaining module-specific work to track under
this ticket. Retired with nothing left to carry forward.
