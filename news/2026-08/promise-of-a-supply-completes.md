# `Promise($supply)` completes when the supply does

`Promise($supply)` is kept when the supply is **done**, with the final value it
emitted — and a `supply { ... }` block is done once all of its `whenever`s have
completed, without anyone writing an explicit `done`. mutsu got neither half
right:

```raku
my $a = Promise.new;
start { sleep 0.2; $a.keep(1) }
my $s = supply { whenever $a -> $v { emit "got $v" } };
say await Promise($s);      # raku: "got 1"   mutsu: Nil, after 30 seconds
```

The drive loop treated "every subscription finished" as *failure to resolve*
and kept the promise with `Nil` — and only after waiting out the whole
thirty-second safety deadline, because the check that noticed it sat behind the
deadline test. So a supply that had emitted was reported as though it never
had.

Two things were needed. The loop now knows the **emitter supplier** the supply
body's `emit` writes to (threaded through `SupplyDrivePolicy::Promise`), so
when the last `whenever` finishes it `done`s that supplier — which resolves the
promise through the registry with the supplier's final emitted value, exactly
as an explicit `done` inside the body would have. And a `whenever` registered
from *inside* another `whenever`'s body was being mistaken for an emitted
value: the Promise policy captures each callback's emits by pushing a
`supply_emit_buffer` frame around it, and the nested `whenever`'s subscription
marker lands in that same frame, so `emitted.last()` made the marker itself the
promise's result. Markers are now split out and handed to the drive loop's
adoption queue (`news/2026-08/react-adopts-nested-whenever.md`), which drives
them like any other subscription.

## What this unblocks

Every `Cro::HTTP::Client` request is exactly this shape:

```raku
Promise(supply {
    whenever self!get-pipeline(...) -> $pipeline {
        whenever $pipeline.send-request($request-object) { ... emit $response }
    }
})
```

so it resolved with `Any` the moment the connection was established, and every
client-side Cro test died on `No such method 'status' for invocant of type
'Any'`. **A mutsu Cro client now completes a real HTTP round trip against a
mutsu Cro server in the same process** and reads the response status and
headers:

```
status = 200
content-length = 5
content-type = text/plain; charset=utf-8
```

The response *body* is still empty (`body-blob` has 0 elements), so the
client-side suite files remain red — the body arrives on the connection after
the headers and its Supply is not being fed. That is the next thread to pull.

Pin: `t/promise-of-supply-completion.t` (six shapes, including two levels of
nesting and an explicit `done`, all verified against `raku` first).
