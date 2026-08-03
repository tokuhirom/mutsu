# A `Supplier` tapped on one thread does not receive an emit from a Cro connection thread

`Cro::HTTP::Middleware::Conditional` (and `::RequestResponse`) pair a request-side
transform with a response-side one and hand the "skip the rest of the pipeline"
signal between them over a `Supplier` held in per-connection state:

```raku
my class SkipPipelineState {
    has Supplier $.early-responses .= new;
}
```

The request side does `$connection-state.early-responses.emit: EarlyResponse.new(...)`
and the response side does `whenever $connection-state.early-responses -> $skipped { … }`.
On mutsu the emit never arrives, so a middleware that answers a request early
(the classic "403 without an Authorization header") produces no response at all:
the client's `await` resolves to `Nil` and the *next* request on that server never
even reaches the middleware.

This is what fails subtests 3 and 4 of the vendored Cro suite's
`http-middleware.rakutest`. Subtests 1 and 2 pass (see
`news/2026-08/nested-whenever-emitter-ownership.md` and
`news/2026-08/object-hash-key-type-survives-parameter-binding.md`).

## What has been measured

Instrumenting the staged `Cro/HTTP/Middleware.rakumod` (`tmp/cro-work/…`, notes
gated on `CRODBG`) with `$*THREAD.id` and `.WHICH` gives, for
`tmp/cond1.p6` (a server with `before => ForbiddenWithoutAuthHeader`):

```
[COND-REQ]  transformer   cs=SkipPipelineState|1782                       thr=1
[COND-RESP] transformer   cs=SkipPipelineState|1782                       thr=1
[COND-RESP] body entered  supplier=Supplier|1781  supply=Supply|1842      thr=1
  [cond] has-header=False
[COND-REQ]  got Cro::HTTP::Response                                       thr=9
[COND-REQ]  early response to supplier=Supplier|1781 supply=Supply|2157   thr=9
[COND-REQ]  LOCAL PROBE TAP fired                                         thr=9
```

Both stages are handed the *same* `SkipPipelineState` (same `.WHICH`) and both see
the same `Supplier` `.WHICH`. The whole pipeline is built and tapped on thread 1;
the request itself is processed on the connection thread (9 here, varies).

The decisive probe: a **plain `.tap`** registered on the supplier's `.Supply`

- from thread 1, right where the response transformer's supply body runs, **never
  fires**;
- from thread 9, immediately before the `emit`, **fires**.

So this is not about the `whenever` machinery — an ordinary tap is lost too. The
`Supplier` object the connection thread holds behaves as a *different* object from
the one thread 1 tapped, while reporting the same `.WHICH`. `Supplier.emit`
dispatches only to the taps stored in the instance's own `taps` attribute
(`src/runtime/native_supply_mut_methods.rs`, the `"emit"` arm), so a thread that
holds a copied attribute map dispatches to an empty tap list. mutsu's `.WHICH` for
an instance is derived from its instance id, which a copy preserves — which is
why every identity check in the trace looks correct.

## Why this is a deep ticket

The suspected mechanism is the thread-boundary env/value clone
(`src/runtime/runtime_thread.rs`, `clone_for_thread` and friends). If an
`Instance`'s attribute map is deep-copied when a value crosses into a spawned
thread, then *every* object with shared mutable state silently forks — `Supplier`
is just the case that shows up loudest. Fixing it means deciding which values may
be copied across a thread boundary at all, which is squarely ADR-0001 territory
(the `Arc`/`Gc` container-kind question) rather than a local patch.

## Reproducers, and what does NOT reproduce

`tmp/cond1.p6` — the failing server. Run with
`CRODBG=1 BMPORT=<port> target/debug/mutsu $(cat tmp/cro-work/inc-paths.txt) -I tmp/cro-work/C_RO_CRO_HTTP_*/lib -I tmp/cro-work/C_RO_CRO_HTTP_*/t tmp/cond1.p6`
and against `raku` with `tmp/crorunraku.sh` for the expected trace.

Every synthetic narrowing tried so far is **green on mutsu**, so do not chase a
smaller repro by guessing — grow `tmp/cond1.p6` down, or break in the debugger on
the `"emit"` arm of `native_supply_mut_methods.rs` and compare the `taps` array
identity between the two threads:

- `tmp/condA.p6` — two `whenever`s in one supply block, one on a shared `Supplier`.
- `tmp/condB.p6` — the same, with an on-demand upstream that delivers synchronously
  during the second `whenever`'s registration.
- `tmp/condC.p6` — the **real** `Cro::HTTP::Middleware::Conditional` driven by a
  hand-built pipeline instead of a server. Passes, and its trace is
  step-for-step identical to the server's up to the missing delivery.
- `tmp/condD.p6` / `tmp/condH.p6` — cross-thread emit into a `Supplier` tapped on
  the main thread, directly and through an object attribute.
- `tmp/condE.p6` / `tmp/condG.p6` — the pipeline built and tapped inside a
  `whenever` body of a `react` block (Cro's connection-manager shape), then
  driven from another thread, sunk by `.tap` and by `whenever` respectively.
- `tmp/condF.p6` — object identity through an object hash (`%h{Mu}`).

## Blast radius

`http-middleware` subtests 3-4, and anything else in the vendored Cro suite that
relies on `Cro::ConnectionState` to share mutable state between the request and
response halves of a pipeline: `http-session-inmemory`,
`http-session-persistent`, `router-auth`, `http-auth-basic*`.
