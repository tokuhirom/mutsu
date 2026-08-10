# Supplier::Preserving keeps its backlog when done is called through an accessor chain

`mutsu` has two "done" lanes for a `Supplier`: an immutable lane
(`native_supplier`, reached when the method call is chained off an
attribute accessor, e.g. `$holder.body.done`) and a mutable lane
(`native_supplier_mut`, reached when the call is on a bare variable, e.g.
`$s.done`). The mutable lane already special-cased `Supplier::Preserving`
to skip its `supplier_reset()` call at done time, so a late tap could
still replay the buffered backlog and the terminal `done`. The immutable
lane did not: it unconditionally reset the state, so a
`Supplier::Preserving` reached through an accessor chain lost its
backlog and done flag the moment `done` was called, before any consumer
had a chance to tap it.

This mattered in practice for `Cro::HTTP2::GeneralParser`, whose
per-stream body `Supplier::Preserving` is stored on a `Stream` object and
driven entirely through `$stream.body.emit(...)` / `$stream.body.done` —
an accessor chain by construction. A response or request body parsed and
finished before the consumer called `.body-blob` (which taps the supply
via `Promise(supply { whenever ... })`) simply vanished: the tap replayed
nothing, and the drive loop waiting on the promise blocked until its
internal timeout.

Fixed by mirroring the mutable lane's terminal-delivered marking and
reset guard into the immutable lane (PR #6166). Verified against the
minimal accessor-chain repro and the full HTTP/2 response body chain;
`t/http2-response-parser.rakutest` (Cro::HTTP2 dist test) went from
hanging to 6/6, and `t/http2-request-serializer.rakutest` stabilized at
16/16 across repeated runs. Pinned by
`t/supplier-preserving-accessor-chain-done-replay.t` (checked against
`raku` too).

A related failure in `t/http2-request-parser.rakutest` (a two-concurrent-HTTP/2-stream
subtest) was investigated as part of this work but turned out to be a
**different** bug: the body-blob's `Promise(supply { whenever ... })`
coercion returned an empty buffer even though the backlog was present,
which traces to the still-open
`todo/tickets/promise-supply-coercion-drives-react-on-calling-thread.md`
(on-demand `Supply.Promise` driving its react loop synchronously on the
calling thread) rather than to this reset bug. That ticket remains open.
