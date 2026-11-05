# Block phasers run compiled and write to the block's captured variables

A `LAST` / `FIRST` / `NEXT` / ... phaser that the block's loop (or
`Block.callable_for_phaser` / `.phasers`) runs from outside is no part of the
block's own chunk, so nothing captured the enclosing variables its body names:
`LAST $ranLAST = True` in a `map` callback lost the write once the body no
longer ran through the tree-walk carrier. The compiler now probe-compiles such a
phaser body and forces the capture of each free variable it names, written ones
as written (#15473), and the phaser runs the shared compiled chunk for its text
(`compile_phaser_body`). Roast's `S32-list/map.t` and the `map-signature-last-*`
tests no longer reach the carrier. A slice of #14572.
