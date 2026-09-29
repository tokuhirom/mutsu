# WhateverCode subscripts run the closure's compiled bytecode

`@a[*-1]` and `@a[*-3..*-1]` used to compile the WhateverCode's AST body
again on every access (issue #10118). Each subscript site that resolves a
Callable index — the array/Seq/Range/Str/hash-pair read arms of
`exec_index_op_with_positional`, the multi-dimensional walk, the
subscript-receiver producer, element and slice assignment (plain and typed
arrays), and `:delete` — bound the element count into a copy of the closure's
captured environment and called `eval_block_value(&data.body)`, which lowers
the body to bytecode before running it. The closure already carried that
bytecode in `SubData::compiled_code`.

All of those sites now call one helper, `Interpreter::call_subscript_code`
(`src/vm/vm_whatever_code_call.rs`), which passes the element count as an
argument for each parameter and runs `compiled_code` through
`call_compiled_closure`, the way any other closure call does. The AST is
evaluated only for a `Sub` value that has no bytecode at all.

`Buf.subbuf(*-2)` / `.subbuf(1, *-1)` were worse: the pure builtin cascade
cannot call a closure, so `builtins::methods_narg::buf` built a whole new
`Interpreter` per call to evaluate the body. The VM now resolves a Callable
offset (and a Callable end index, which becomes a length) to an `Int` before
the cascade runs, and the per-call interpreter is gone.

Measured with a gdb breakpoint on `Compiler::compile` over a loop mixing
`@a[*-1]` reads, `@a[*-1] =` / `@a[*-2] =` stores, `:delete`, `Seq[*-1]`,
`@a[*-2..*-1]` and `subbuf(*-2)`: 7 compiles per iteration before, 0 after.
