# Cro's middleware suite dies coercing Any into Promise on a cached response

The vendored Cro suite's `http-middleware.rakutest` subtest 4
("Request/response middleware using `Cro::HTTP::Middleware::RequestResponse`")
runs its first five assertions green and then dies:

```
    ok 1 - Got 200 response on first request
    ok 2 - Response part added header
    ok 3 - Expected body
    ok 4 - Got 200 response on second request
    ok 5 - Response part did not run on early response
    # subtest died: Impossible coercion from 'Any' into 'Promise': no acceptable
    coercion method found
```

(the diagnostic comes from `news/2026-08/subtest-reports-why-its-body-died.md`).
The statement it dies on is the sixth assertion,

```raku
is await($resp.body-text), '1', 'Got cached body';
```

i.e. the `body-text` of the **second** request — the one `OverlySimpleCache`
answers early, from its cache, without running the inner route. `Any` where a
`Promise` is required is an `await` on a response whose body promise was never
made, so the early-response path hands back a `Cro::HTTP::Response` with no body
supply attached.

This was previously seen as an *intermittent* death in subtest 3
("Conditional response middleware"), which is the same early-response shape; the
old ticket
(`cro-conditional-middleware-subtest-intermittent-promise-coercion.md`) is
superseded by this one now that it reproduces deterministically here. It is the
only remaining failure in the file: with the 2026-08-04 closure-capture and
atomic-scalar fixes in, subtests 1-3 and 5-11 pass on every run and the counter
that used to answer `0`/`3` now answers `1`, `2`.

## Where to look

The early response is produced by a `before-matched` middleware that sets
`$response.status` and body itself instead of letting the request reach the
route. The three 2026-08-04 supply fixes touch exactly this area and are worth
re-reading first:

- `news/2026-08/channel-backed-whenever-source-is-not-finite.md` (a supply over
  a socket used to complete at tap time)
- `news/2026-08/done-in-a-tap-callback-is-a-control-signal.md` (a `done` that
  lost its control flag)
- `todo/tickets/done-in-a-whenever-body-does-not-stop-later-emits.md` (the half
  of `done` semantics still missing: a completed supply's source keeps
  delivering)

## How to see it

```
bash tmp/cro-one.sh http t/http-middleware.rakutest release 2>&1 |
  grep -nE 'subtest died|^not ok'
```

Deterministic across runs.
