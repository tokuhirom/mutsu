# A readonly assignment runs its right-hand side first

`sub f($x) { $x = g() }` used to die before calling `g`: the compiler emitted
`CheckReadOnly` ahead of the RHS. Rakudo evaluates the RHS and then fails the
assignment, so `g`'s side effects happen. `CheckReadOnly` now sits after the
RHS, immediately before the store it guards — in the fused `~=` and `OP=`
forms too.

That placement also retires `Interpreter::pending_sigilless_store`, the name a
`CheckReadOnly` that let an assignment to a sigilless term through (its bound
object has a user `STORE`, or it is a mutable Array/Hash) parked for the store
that followed. The verdict is now the `SIGILLESS_STORE` bit of the mark-context
word, consumed by that very next store and isolated across calls like the rest
of the family. ADR-10779 D3: `Interpreter` loses a field.
