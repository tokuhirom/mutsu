# A read of a plain `@`/`%` local takes the GetLocal fast path

`GetLocal` has a fast path (#8332) that reduces the opcode to cloning the slot onto the stack
when every guard of its long probe chain is known to decline. It was closed to every `@`/`%`
local, because two of those guards are keyed on the sigil: the atomic-lane probe and the
thread-clone shared-snapshot read. So each `@arr[$i]` paid ~290 instructions in the `GetLocal`
alone before the subscript ran.

Both guards are runtime facts with a one-load answer: whether any atomic-lane entry exists in the
process, and whether this interpreter is a thread clone. The read now asks those two and takes the
fast path otherwise, for a slot holding an `Array`/`Hash` node or a binding cell. The rest of the
chain (attribute cells, CStruct fields, overlay-cell adoption) cannot answer for such a slot.

Measured with callgrind on 100,000-iteration loops (#19286): a plain `@arr[$i]` read costs ~195
fewer instructions (marginal 1,100 → 905), a `:=`-bound one ~156 fewer, and the read half of
`bench-threads-serial` ~150 fewer per iteration.
