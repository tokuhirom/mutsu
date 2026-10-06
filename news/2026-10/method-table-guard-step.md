# Built-in method rows get one guard step, named arguments and interpreter handlers

ADR-11276 slice 3A. The method table stops being a list of pure zero-argument lookups on nine
receiver shapes and becomes the place every kind of built-in method can be registered.

A row now declares the named arguments it binds, and may be a `Named` handler, or an `Interp`
handler that calls closures or reads dynamic variables. One guard step splits named arguments
from positional ones, finds the row by its positional arity, and admits the arguments by the
row's flags, so a `Junction`, a lazy `Seq` or a user instance still takes the cascades. The
first proof rows are `flat(:hammer)`, `List.combinations` with a `Range`, and `Any.collate`,
which reads `$*COLLATION`.

Fourteen more receivers have a shape: `Bool`, `Range`, `Pair`, `Capture`, `Version`, `Uni`, the
six quant hashes, `Date` and `DateTime`. A new shape starts closed, so a row owned by an ancestor
(`Any.elems`) cannot answer it before its owner slice has audited that row. The type object of a
built-in type is a receiver too, and answers only a row that says so: `Int.Bool` is `False`.

The rows moved into one directory per slice group, so the six group slices that follow can run in
parallel without editing a shared list. `scripts/method-rows-report.py` prints the arms left in
the cascades and the rows registered per group.
