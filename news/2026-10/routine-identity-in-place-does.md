# A routine has identity: `does` on it composes in place

In Raku a `Routine` is an object, so `&f does R` is seen by every alias of
`&f`. mutsu used to build a new mixin value and rebind only the variable the
`does` was written on, so an alias taken earlier, a role argument that had
captured the routine, and a closure's `self` all kept the plain routine.

Following ADR-11827, every value of one routine now shares a composition
cell (owned by a named routine's definition, so registry rebuilds of `&f`
share it too). `does` composes onto the routine's current composition and
stores the result in the cell; method dispatch and role checks on a routine
view it through the cell. Successive `does` keep one role-attribute store,
and `.clone` gives the copy its own cell.

This is what upstream NativeCall's `is native is symbol('strlen')` relies on:
the `Native` role keeps the routine as `$routine`, and the later
`NativeCallSymbol` composition is now visible through it.

The same work fixed a nondeterministic read: a closure returned by a role
method reading `$!a` on a value or routine composing several roles picked a
role-attribute key in hash order, so it sometimes saw the construction seed
instead of the value the role's method had written. It now reads the key of
the role that declares the attribute.
