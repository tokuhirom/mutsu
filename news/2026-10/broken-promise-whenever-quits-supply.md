# A broken promise under `whenever` quits its supply instead of hanging

`Promise(supply { whenever $promise { … } })` over a promise that breaks used to wait out the
supply drive's 30-second deadline and then come back *Kept*, so `try await` left `$!` unset.
Cro's HTTP client is built from exactly that shape, which is why
`try await Cro::HTTP::Client.new.post("http://127.0.0.1:1/")` took 30 s to report nothing
(#12789). Five gaps lined up:

- On the `Promise(supply)` path a `whenever $promise` source emitted a broken promise's cause
  as an ordinary value. It now quits, through one helper the react loop shares
  (`promise_source_events`), and the cause arrives as an `X::AdHoc` exception, as `.cause`
  hands it out.
- Under the Promise drive policy a source's quit only retired its subscription. It now runs
  the whenever's `QUIT` phaser, or breaks the promise when there is none.
- The `whenever $promise` stand-ins a nested `supply { }` stage parks for arming stayed on the
  calling interpreter, while the drive loop runs on a thread clone, so they never fired.
- A `QUIT` phaser that matches no `when`/`default` handled the quit anyway. Like `CATCH`, it
  now rethrows, which quits the enclosing `react`/`supply` block.
- `IO::Socket::Async.connect` connected on the calling thread and returned an already-broken
  promise for a refused port. It now connects on a helper thread, as rakudo connects on its
  event loop, so a caller that subscribes and then sends (Cro's pipeline) sees the failure
  after its request is queued.

The Cro request now fails in about 0.13 s with the connect error, as under rakudo (0.19 s).
When the refusal still lands before the request is queued — a race in Cro's own pipeline,
open under rakudo too — it fails just as fast with `PipelineClosedBeforeHeaders`.
