# Supplier.emit rejects a named argument as the emitted value

`$supplier.emit(a => 1)` passes `a => 1` as a named argument, so `emit` has no positional
value. mutsu used to emit the Pair anyway; it now fails with rakudo's
`Too few positionals passed` error. `emit((a => 1))` still emits the Pair.
