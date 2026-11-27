# TRIR fuses table tests, codepoint tests and boxed slot reads

Four more stack sequences of the TRIR switch loop are folded by the peephole
pass into one op that touches fewer banks:

- `AtPosIOuter(o); JumpIf..I(t)`: a character-class table test such as
  `nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ...)`. It is now
  `JumpAtPosIOuter`, which pops the index and branches on the table entry.
- `OrdAtLocalRef` or `OrdAtLocalSlot` followed by `JumpCmpC`: a codepoint
  compared with a constant. It is now `JumpCmpOrdAtRefC` / `JumpCmpOrdAtSlotC`,
  which compares the codepoint where it is read.
- `LoadObj(a); StoreObj(b)` is `CopyObjLocal`, with no push or pop.
- `LoadObj(n); UnboxI` is `UnboxILocal`, which reads the slot in place instead
  of cloning it.

Inlining renames the new ops like the ones they replace. An inlined callee's
table index is shifted past the caller's own free variables, as
`AtPosIOuter`'s is.

On the 100-record SPDX decode of `bench-json-fast-spdx`, the `from-json` call
drops from 59.14M to 57.97M instructions (-2.0%). Refs #8673.
