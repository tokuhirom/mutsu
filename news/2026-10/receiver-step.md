# One receiver step for every method dispatch entry

Before a method call reaches the built-in method table, its invocant may be in
a state the rows do not read directly: a `Scalar` container or an itemized
`Array`/`Hash` (transparent except for `.VAR`, `.raku`, `.item`, `.self`), a
hash-like receiver or a `Match` answering `Any`'s `self.list.METHOD` methods
on their list, an `Array`-valued enum member answering its base value's
collection methods, a user `Channel` subclass coercing the channel it
carries, and a user `IO::Path` subclass answering `Cool` methods on its path.
Each dispatch entry used to spell these steps itself: the VM's native entry,
the by-name entry and the prologues of the zero- and one-argument cascades,
and the copies had drifted (the native entry skipped the container steps, the
cascades skipped the enum and `IO::Path` ones). `method_table::receiver_step`
now decides them once, and every entry acts on its answer (#14080, ADR-11276
§2.4).
