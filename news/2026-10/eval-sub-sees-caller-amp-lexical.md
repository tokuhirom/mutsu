# A sub declared inside `EVAL` sees the EVAL caller's `my &g`

`{ my &g = -> { 'lexical' }; EVAL q[sub z { g() }; z()] }` called an outer
`sub g` instead of the caller's `my &g`, both for a bare `g()` and for `&g()`.
`EVAL` compiles in its caller's lexical scope, but its compiler is a fresh one
that cannot see the caller's scopes, so the EVAL-declared `sub z` never
recorded `&g` as a free variable: the bare call only uses an env `&g` that the
code captured, and `&g()` treated the caller's binding (from another
compilation unit) as merely inherited (#10638) and fell back to the routine.

`compile_block_value_opts` now seeds an EVAL unit's compiler with the
`&`-lexicals visible at the call site — every plain user `&name` in the env
whose callable is a lexical binding rather than the routine of that name — as
its `outer_code_var_names`, the same set a sub written inline would inherit
(#11154).
