# A `whenever` source's `.on-close` runs when the supply block stops

`$supplier.Supply.on-close({ ... })` attaches a callback that should run when
the consumer stops listening. Consumed through a `whenever` in a
`supply { }` block, that callback never ran: not when the block's tap was
closed, and not when the source completed normally. The block's own
supply-level `done` was the only path that fired it.

The supplier-backed `whenever` registration used to collect the source's
on-close callbacks into a list that only an explicit `done` fired. It now
registers them beside the block's CLOSE phasers, on the block emitter's close
list. Every way the block can end takes that list, so the callbacks run once
whichever comes first:

- a `Tap.close`, through the tap's `close_supplier_id`;
- a source finishing normally, through the close marker on each source's
  done;
- a `done`, through the completion marker, which now takes the same list.

The Stomp distribution's `t/server.rakutest` surfaced this (#10833).
`Test::IO::Socket::Async`'s listener is
`$supplier.Supply.on-close({ $!is-closed-vow.keep(True) })`, and
`Stomp::Server.listen` consumes it in a `whenever`. The test's
`await $listener.is-closed` hung. That file now passes 28 of 28.
