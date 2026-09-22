# Function composition (`o`/`∘`) works as the inner op of a Z/X meta-operator

`o` (and its Unicode alias `∘`) already worked as a plain infix
(`&f o &g`), but not as the inner operator of a `Z`/`X` meta-operator or a
reduction: `(&f, &g) Zo (&g, &f)` died `Unsupported reduction operator: o`.

The plain infix compiles straight to a dedicated `OpCode::FunctionCompose`,
which calls `Interpreter::compose_callables` directly. A meta-operator's
per-pair leaf, `eval_infix_leaf` (`src/vm/vm_dispatch_helpers.rs`), never
routed through that opcode at all -- it always fell to the static,
`&self`-free `apply_reduction_op` table, which has no entry for `o` and
never could, since composing two Callables needs `self` to resolve the
composed sub's parameter signature.

`eval_infix_leaf` already special-cases a handful of operators that need
`self` ahead of the static table (`eqv`, `=~=`, the numeric comparisons,
container identity); `o`/`∘` now joins them, calling `compose_callables`
directly. This fixes `Zo`/`Xo`, `Z∘`/`X∘`, and `[o]` reductions all at once,
since they all route through the same leaf dispatch.

Pinned by `t/lang/operators/metaop-function-compose-inner-op.t`.

Closes #9050.
