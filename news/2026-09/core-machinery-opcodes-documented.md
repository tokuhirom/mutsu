# Core-machinery opcodes documented

The generated opcode reference (`site/opcodes.html`) renders each `OpCode`
variant's `///` doc comment from `src/opcode.rs`. The 31 opcodes a reader meets
first had none, so the page listed them as undocumented: the constant loads
(`LoadConst`, `LoadNil`, `LoadTrue`, `LoadFalse`), the variable reads and
writes (`GetLocal`, `SetLocal`, `GetGlobal`, `SetGlobal`, `GetArrayVar`,
`GetHashVar`, `GetBareWord`, `GetPseudoStash`, `SetVarType`), the topic ops
(`SetTopic`, `SaveTopic`, `RestoreTopic`, `ExitPointyTopic`), `Dup`/`Pop`, the
jumps, `last`/`next`/`redo`, `proceed`/`succeed`, and the compound
`Given`/`When`/`Default`/`RepeatLoop` ops.

Each now has a doc comment giving its stack effect, what its operands index
(a constant-pool index, a local slot, or an absolute jump target), the Raku
construct the compiler emits it for, and the semantics that its name does not
suggest. These were read off the dispatch arms and `exec_*_op` handlers rather
than guessed from the names. Examples: `JumpIfTrue` peeks at the top of the
stack where `JumpIfFalse` pops it, which is how `||` keeps its left operand as
the result. Jump targets are absolute op indices, not offsets. `Given` always
nets exactly one stack value. A matching `when` exits by raising the succeed
signal and does not push. The compound ops' struct fields carry their own
docs, which describe the region layout (`[RepeatLoop] [body..] ← cond_end
[cond..] ← body_end`).

Tracked as #9441. Sibling tickets cover the remaining opcode families.
