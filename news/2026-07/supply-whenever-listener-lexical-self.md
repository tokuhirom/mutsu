# Supply-block whenever over TCP listeners; lexical `self` in interpreter closures

Three fixes that move Cro::Core's `t/tcp.rakutest` from 14 to 21+ passing
tests (from aborting at the first real-socket test to completing both
`Server connection` subtests), found by driving the vendored Cro::Core suite:

1. **`whenever IO::Socket::Async.listen(...)` inside a `supply` block now
   works under a plain `.tap`.** The listener's subscription marker was a
   2-element array that only the react event loop understood; the supply-block
   tap path recognises only 4-element `[source, body, [LAST…], [QUIT…]]`
   markers, so the marker leaked to the tap as a plain emitted value and the
   whenever body never ran. The marker now uses the standard 4-element shape,
   and the supply-block tap path gained a channel-backed arm: a worker thread
   drains the accept channel and drives the whenever body, whose
   `$emitter.emit(...)` reaches the outer tap through the emitter supplier.
   The outer Tap also carries the listener handle so closing it stops the OS
   listener (`Cro::TCP::Listener.incoming` semantics).

2. **In-memory socket supplies no longer collide with real Suppliers.**
   `async_socket_supply_in_memory` (and the UDP twin) stamped their Supply's
   `supplier_id` attribute with an id from `next_supply_id()` — a separate
   counter from `next_supplier_id()`, both starting at 1. A genuine
   `Supplier.new` with the same number cross-delivered its emissions into the
   socket's tap (Cro::TCP::Message objects arriving on a client's
   `.Supply(:bin)` tap). Both sites now allocate from the supplier counter.

3. **`self` is lexical in the interpreter closure path.** The `merge_all`
   captured-env merge in `call_sub_value` used don't-overwrite for `self`, so
   a natively invoked callback (a whenever body dispatched from a supplier tap
   or the in-process connect path) resolved `$.attr` against whatever `self`
   the caller env last leaked — the second `Cro::TCP::Listener` in a process
   read `$.nodelay` off a `Cro::TCP::Replier` and died (swallowed), hanging
   the test. The VM closure dispatch already force-installed the captured
   `self` (the DBDish lesson); the interpreter path now mirrors it.

Remaining in tcp.rakutest: the `:nodelay` subtest needs `.native-descriptor`
on in-memory sockets (`todo/tickets/in-memory-socket-native-descriptor.md`).

Pins: `t/supply-whenever-listener-tap.t`, `t/supplier-id-socket-collision.t`,
`t/closure-lexical-self-native-dispatch.t`.
