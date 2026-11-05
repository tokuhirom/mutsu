# Reading a captured container's cell no longer clones the cell handle

Since every escaping-captured `@`/`%` has a declaration-site cell (ADR-16595
slice 1), a closure reads such a container through `Value::into_deref`, which
cloned the cell's `Gc` handle (and dropped it again) before it could look at the
value. The common read, a cell holding a plain value, now takes the value
through the borrowed handle and clones it only when the cell holds another cell.
