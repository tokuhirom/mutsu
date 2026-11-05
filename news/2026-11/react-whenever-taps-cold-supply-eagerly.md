# A react `whenever` taps a cold `supply { }` source when it runs

In `react`, a `whenever` over a cold `supply { ... }` now runs the supply body at
`whenever` time, as rakudo does, instead of after the react body finishes. The
drive loop adopts the pre-run result. This fixes `connect` right after a
`start react { whenever IO::Socket::Async::SSL.listen(...) }` being refused,
because the listener is now bound before the react body continues (#15958).
