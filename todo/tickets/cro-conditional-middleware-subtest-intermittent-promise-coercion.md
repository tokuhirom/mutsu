# Cro's Conditional-middleware subtest intermittently dies coercing Any into Promise

The vendored Cro suite's `http-middleware.rakutest` subtest 3 ("Conditional
response middleware using Cro::HTTP::Middleware::Conditional") passes on most
runs but sometimes dies partway with

```
    # subtest died: Impossible coercion from 'Any' into 'Promise': no acceptable
    coercion method found
```

(the diagnostic comes from `news/2026-08/subtest-reports-why-its-body-died.md`).
When it dies it has already logged its earlier assertions; the failure is not in
a particular assertion but in the subtest body between them.

`Any` where a `Promise` is required is the shape of an `await` on something that
never got its promise — most likely a response (or a `body-text`) that arrived
as `Nil` because a pipeline stage completed early. The three fixes landed the
same day that touched exactly that area are worth re-reading first:

- `news/2026-08/channel-backed-whenever-source-is-not-finite.md` (a supply over
  a socket used to complete at tap time)
- `news/2026-08/done-in-a-tap-callback-is-a-control-signal.md` (a `done` that
  lost its control flag)
- `todo/tickets/done-in-a-whenever-body-does-not-stop-later-emits.md` (the
  half of `done` semantics still missing: the source keeps delivering)

The last of those is the most suspicious: a `whenever` body that keeps running
after its supply completed can still take side effects, and the Conditional
middleware's request and response halves share a `Supplier` whose `done` is
driven by a LAST phaser.

## How to see it

```
D=$(echo tmp/cro-work/C_RO_CRO_HTTP_*)
for i in 1 2 3 4 5; do
  timeout 300 target/debug/mutsu $(cat tmp/cro-work/inc-paths.txt) \
      -I "$D/lib" -I "$D/t" "$D/t/http-middleware.rakutest" 2>&1 |
    grep -E '^(ok|not ok) 3 '
done
```

Roughly one run in several reports `not ok 3`; the rest report `ok 3` with all
six of its assertions.
