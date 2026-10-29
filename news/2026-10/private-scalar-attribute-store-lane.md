# A private scalar attribute store takes a direct lane

`$!x = <scalar>` inside a method used to run the whole by-name store ladder:
the declared type came from the registry, the value was written to the frame's
env under both twigil spellings (a `format!` and two inserts), and only then was
`self`'s attribute cell updated. For an inert payload (`Int`, `Num`, `Str`,
`Bool`, ...) into an attribute whose declaration is untyped or a native or core
scalar type the value satisfies, that reduces to one write into the attribute
cell and the frame's local slot.

The layout of a class now records, per declared slot, what a store has to check
(`AttrStoreKind`: untyped, `int`/`str`/`num`, `Int`/`Str`/`Num`/`Bool`, or
"decline"), and `exec_set_local_attr_fast` uses the site's `(layout, slot)` cache
to write the cell directly. Class constraints, `where` clauses, smileys,
`@`/`%` attributes, bound or promoted cells, role methods and cold sites keep
the general path.

On the `profiling` build (callgrind slope, caller a plain loop) the store
`method set($k) { $!key = $k; self }` goes from 26.3k to 18.8k Ir per call
(the empty-body method is 15.0k).
