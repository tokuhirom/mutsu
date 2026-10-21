# An imported `is rw` routine keeps its Proxy at a lexical-import call site

A call of a routine a block's `use` imported (`LexImportCall`, and any `&name(...)` call on a code
value) fetched a returned `Proxy` unconditionally: `exec_call_on_code_var_op` ended with a second
`maybe_fetch_rw_proxy(result, true)` that undid its own per-branch decision, and its routine branch
passed `true` for an `is rw` routine. So `with substr($x, 1, 2) { $_ = 'a' }` with a block-scoped
`use P5substr` assigned to a copy and left `$x` alone (#14361). The final fetch now follows each
branch's choice, the routine branch fetches only for a routine that does not return a container,
and a multi dispatcher whose captured candidates include an `is rw` one hands its result on
un-fetched too.
