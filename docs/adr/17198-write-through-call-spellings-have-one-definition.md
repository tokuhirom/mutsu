# ADR-17198: The write-through call spellings have one definition, then a typed IR

- **Status**: Proposed
- **Date**: 2026-11-08
- **Deciders**: Claude (lane:rakuast worker 23), for the maintainer's review
- **Issue**: [#17198](https://github.com/tokuhirom/mutsu/issues/17198) (reading (A), family 2 of [#15761](https://github.com/tokuhirom/mutsu/issues/15761))
- **Related**: [ADR-15761](15761-param-placeholders-are-typed.md) (family 1), [ADR-10723](10723-rakuast-is-the-frontend-ir.md),
  [ADR-15334](15334-stage-2-parser-builds-typed-rakuast-nodes.md) Amendments 4 and 5

## 1. Context

Assigning through a method call (`$o.attr = 1`, `$o.m(...)[$i] = 1`), a named rw routine
(`f(...) = 1`, `f(...)++`) or a callable (`$code(...) = 1`, `(...) = ...` over literal lvalues) is
lowered, by the parser and by `rakuast`, into a **call whose callee is a reserved name**:

| callee | arguments |
| --- | --- |
| `__mutsu_assign_method_lvalue` | invocant, method name, method arguments, value, target variable or Nil, preserve-hash-entries flag, dot-twigil flag (the last three optional) |
| `__mutsu_assign_named_sub_lvalue` | routine name, call arguments, value |
| `__mutsu_assign_callable_lvalue` | callable, call arguments, value |
| `__mutsu_incdec_named_sub_lvalue` | routine name, call arguments, increment kind |
| `__mutsu_index_assign_method_lvalue` | invocant, method name, method arguments, index, value |
| `__mutsu_index_delete_method_lvalue` | invocant, method name, method arguments, index |

The name is **executable IR**, not a private channel between the parser and the `.AST` view:
measured 2026-11-08 on `main`, 59 quoted spellings in 22 files across `src/compiler`, `src/vm`,
`src/runtime`, `src/trir` and `src/rakuast` compare or produce it by string. The argument layout
is likewise known to each consumer separately (`args[3]` is the value here, `args.len() != 3` there).
A new write form, or a new argument, has to be found by `grep` in all of them.

## 2. Decision

Three steps, one PR each; this ADR is accepted per step, the first being ready now.

1. **One definition (this PR).** `src/lvalue_call.rs`, a leaf module like `qualified` and `meta_ns`,
   owns the six spellings as constants and documents the argument layout. No producer or consumer
   spells one: they use `crate::lvalue_call::ASSIGN_METHOD` and kin (also as `match` patterns). A
   ratchet in `check-frontend-debt` counts quoted spellings outside the module; its ceiling is 0.
   No behaviour change and no precompilation-cache bump (the name is serialized, its value is
   unchanged).
2. **One constructor and one reader.** (Constructors landed for all six forms. The compile-side reader landed too: `lvalue_call::classify` answers an `LvalueCall` with named arguments for the assign-method, assign-named-sub, assign-callable and incdec forms, and the compiler, the TRIR front end and `rakuast/assign_writeback.rs` use it instead of `args[n]`. The VM and the runtime builtins read run-time values and the two index forms, which are next.) `lvalue_call` gains constructors for each form
   (`method(invocant, name, args, value)`) and a classifier returning the form and a typed view of the
   arguments, so the argument order is written once and a consumer reads `view.value()` rather than
   `args[3]`. The producers (`parser`, `rakuast/assign_writeback.rs`, `assign_target.rs`,
   `temporize.rs`, `hyper_assign.rs`) build through the constructors.
3. **Typed `Expr` variant.** The compiler consumes `Expr::LvalueCall { form, .. }` instead of a
   `Expr::Call` with a reserved callee; the runtime builtins keep the name only as the entry point of
   the builtin function. This changes the serialized `Expr` shape, so it bumps the precompilation cache
   in its PR (`src/precomp.rs`).

## 3. Consequences

- Adding or renaming a write-through form touches `lvalue_call` first; the ratchet fails a consumer
  that spells one.
- Steps 2 and 3 are mechanical but wide (about 100 occurrences); doing step 1 first keeps each of them
  reviewable and lets the compiler, not `grep`, find every site.
- `__mutsu_star_lvalue_rhs` and `__mutsu_logical_lvalue_<n>` (a temporary name) are a different family
  (compiler-internal temporaries) and are not decided here.
