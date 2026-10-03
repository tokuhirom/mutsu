# A module's `my atomicint` holds its initializer when read from an exported routine

Take `unit module AT; my atomicint $initialized = 0; sub f() is export { ⚛$initialized }`.
Calling `f` from the importer used to read the `atomicint` type object rather
than `0`. Even a plain `$initialized` read did, because a read of an
`atomicint` name is routed through the atomic fetch. The value only appeared
after the first `⚛=` store. LogP6 printed `Use of uninitialized value
$already of type atomicint` on every load because of this.

The atomic ops resolve their target by name. They searched the frame's own
locals, `env` (which belongs to the importer) and the package-block lexical
store, and then fell back to a name-keyed lane that had never seen the
variable. A module's file-scope lexical lives in the compunit's unit-lexical
store, which is where a plain read of an untyped lexical already finds it.
`atomic_scalar_cell` now boxes that binding into a shared cell, as it does for
a frame local or a package-block lexical. Every atomic op and every plain read
then share one binding, and concurrent `⚛++` from worker threads counts every
increment (#11455).
