# Proc::Async's methods are rows of the method table

`Proc::Async`'s `start`, `kill`, `write`, `print`/`put`/`say`, `close-stdin`, the `bind-*` methods, `ready`,
`stdout`/`stderr`/`Supply`, `command`, `started`, `w` and `pid` are now rows of `Proc::Async`, found by their owner
(ADR-11276 §9.128). Six name lists and name matches (the instance dispatch, both native dispatchers, the
implementation, and two VM skip lists) are gone; the implementation dispatches on an enum. The process object's state
is its shared attribute cell, so a method called through any alias (a variable, an array element) is seen by all of
them, and no row needs the receiver's place.
