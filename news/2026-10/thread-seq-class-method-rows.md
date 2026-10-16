# Seq.from-loop, Thread.start, X::AdHoc.from-slurpy and Supply.start are method-table rows

`Seq.from-loop`, `Thread.start`, `Thread.is-initial-thread` on the type object, `X::AdHoc.from-slurpy` and the
instance method `Supply.start` are now rows of the classes Rakudo declares them on (ADR-11276 §9.124), found by their
owner instead of by name-matching arms that each tested which type object the receiver was. The `Seq.from_loop`
spelling, which Rakudo does not have, is gone.
