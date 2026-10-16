# Supplier's and the schedulers' methods are rows of the method table

`Supplier`'s `emit`, `done`, `quit` and `Supply`, and the schedulers' `cue`, `loads`, `max_threads`, `usage` and
`uncaught_handler`, are now rows found by their owner (ADR-11276 §9.133). Each row names which native implementation
it runs, so the receiver no longer picks one: a supplier held in an attribute (`$!supplier.emit`) now emits through
the same serialized path as one held in a variable, and reading a scheduler's `uncaught_handler` through a variable can
no longer clear it.
