# `&name` in a routine no longer reads a same-unit caller's `my &name`

`sub h() { &g() }` called from `{ my &g = -> { h() }; g() }` used to resolve
`&g` to the caller block's `my &g` and recurse until the stack overflowed;
`&g.name` likewise reported the caller's binding. The cross-compunit form was
fixed in #10638, but within one compilation unit the by-name env lookup could
not tell an enclosing scope's `&g` from a caller's.

The compiler now answers that question. For every `&g` read
(`GetCodeVar` / `CallOnCodeVar`) whose read site sees no `&g` lexical — no
`my &g`, `&g` parameter, role `&g` type parameter or class-body `&g` in any
enclosing scope — it records the name in the code's `unscoped_amp_reads`, and
`imported_amp_over_inherited` then skips a caller's same-unit env binding in
favour of the declared routine. The record is only made by compilers that see
the whole scope chain (`Compiler::lexical_scope_known`, set for the program's
unit and handed down to its subs, methods and closures); a body compiled out of
context (EVAL, interpret-path recompiles) keeps the by-name resolution.

Making that proof sound exposed one lexical the compiler never saw: a
single-parameter pointy block `-> &f { ... }` dropped the `&` sigil (it took
the sigil-stripping `Expr::Lambda` form), so `&f` lived only in env. It now
takes the `AnonSubParams` path like `-> @x` and `-> %h`, which also fixes a
bare `f()` in such a block calling an outer `my sub f` instead of the argument.
