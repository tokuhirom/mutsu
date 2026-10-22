# `Promise(supply { whenever <static> { whenever ... } })` taps the nested whenever

When a `supply` coerced with `Promise`/`.Promise` replayed a `whenever` over a static source
(`Supply.from-list`), a `whenever` registered inside its body was captured as an emitted value, so the
promise resolved to the registration tuple (or `Nil`). The marker now goes to the drive loop's adoption queue
and the loop is not treated as empty while one is queued, so the nested source is subscribed like a
nested `whenever` under a live source. Fixes #14453.
