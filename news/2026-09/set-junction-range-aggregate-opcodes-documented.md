# Set, junction, range and aggregate opcodes documented

The fourth slice of the opcode reference (`site/opcodes.html`) adds doc
comments to the set operators, the junction builders, the range and sequence
operators, `MakeArray`/`MakeHash` and `but` in `src/opcode.rs`. None of these
had doc comments before.

Each doc names the Raku operator in its Unicode and ASCII spellings
(`∈`/`(elem)`, `∪`/`(|)`, `⊎`/`(+)`, `…`/`...`) and gives the stack effect.
For the binary set operators it says which result type the shared
`runtime::set_op_values` body produces: both operands are promoted to the
higher of Set < Bag < Mix, with `(+)` and `(.)` starting at Bag, and the result
keeps the left operand's mutability (a `SetHash` operand gives a `SetHash`).
For the counted ops (`JunctionAnyN`/`AllN`/`OneN`, `MakeArray`, `MakeHash`) it
says how many values are popped. `MakeHash` counts pairs, so it pops twice its
operand.

One detail came out of reading the compiler rather than the op names. A
two-operand `a | b` compiles to `JunctionAny`, the same op as the sequential
`S|`. Only a chain of three or more compiles to `JunctionAnyN`, and that is
the only form that calls a user `infix:<|>`. The docs record this. I checked
the stated result types against `raku`: `set(1) (+) set(1)` is a `Bag`,
`SetHash ∪ set` is a `SetHash`, and `Int but R` is `Int+{R}`.

Tracked as #9444.
