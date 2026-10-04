# A live supply's `done` reaches every stage of a combinator chain

A `done` on a Supplier finished the suppliers derived from it (by `map`,
`grep`, `head`, `zip`, ...) with only their done callbacks. A stage tapped
on a *derived* supply that acts at done therefore never ran its terminal
step: `$s.Supply.map(...).reduce(...)` never delivered its fold,
`.map(...).batch(...)` dropped its last partial batch, and
`.head(n).reduce(...)` stayed silent.

The terminal path now mirrors the emit path (`supply_emit_drive`): one
driver, `propagate_supplier_done` (`src/runtime/supply_done_drive.rs`),
runs a finished supplier's whole terminal action set -- flushes, done
callbacks, reduce results, zip/merge completion -- and recurses into each
derived supplier through `finish_derived_supplier`. The two copies of that
sequence in `Supplier.done`'s lanes collapsed into a call to it, and a live
`head` reaching its limit finishes its own derived stages too.
