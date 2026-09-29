# ADR-0133: Runtime executes precompiled chunks — no per-call AST compile

- **Status**: Proposed (Slice 1 — signature expressions — implemented with #10107; later slices open)
- **Date**: 2026-09-29
- **Related**: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) (declarations
  compile to plans; D2c compiled attribute defaults as child chunks),
  [#10107](https://github.com/tokuhirom/mutsu/issues/10107)

## Context

ADR-0019 moved *declarations* off the AST: a declaration plan "must not retain the source `Stmt`
or an executable AST body", and runtime-dependent declaration expressions (computed names,
trait arguments, attribute defaults) became child chunks compiled once. It did not cover what
happens when a **call** runs. Runtime values still carry AST that the runtime re-compiles:

- `ParamDef` (shared by `SubData`, `FunctionDef`, `CompiledFunction`, closure signatures) holds
  its `where` clause, default and shape dimensions as `Expr`. The binder evaluated each one via
  `eval_block_value(&[Stmt::Expr(expr.clone())])`: one `AST clone + Compiler::new() + compile`
  on **every** call. A `where * > 0` paid two (the clause, then the WhateverCode body whose parse
  site was new every time, so the carrier cache could never hit).
- `SubData.body` (`Arc<Vec<Stmt>>`) is still read by several value-call paths that ignore the
  `compiled_code` the same `SubData` carries.

Measured on 2f04179a with a gdb breakpoint on `Compiler::compile`, per loop iteration: a sub with
`where * > -1` → 2 compiles, `where { … }` → 1, a multi or method `where` → 2, an omitted
non-literal default → 1, `@a where .elems > 0` → 1, a WhateverCode subscript `@a[*-1]` → 1, a
sequence endpoint `... * > 50` → 1 per generated element, `.subst(/a/, { … }, :g)` → 1 per
match. The #10107 multi-dispatch benchmark section was 46x rakudo, almost all of it compiling.

Three mechanisms already exist that *cache* a runtime compile (`carrier_compile_cache`,
`subset_predicate_cache`, the OTF `FunctionDef` fingerprint cache). A cache keeps the AST as the
runtime's source of truth and has to compute a context key per call; it is a mitigation, not the
end state.

## Decision

1. **The runtime does not compile AST per call, per element or per match.** An expression the
   runtime evaluates repeatedly is compiled by the compiler, in the lexical context that owns it,
   into a child chunk; the runtime runs the chunk through its normal re-entrant bytecode entry.
   Compiling at runtime remains legitimate only where the input *is* source at runtime: `EVAL`,
   module load, `BEGIN`/`CHECK`, REPL, and a declaration executed once.
2. **Where the AST node is shared, the chunk rides on it.** A `ParamDef` carries a
   `ParamCode` — an `Arc<OnceLock<ParamChunks>>` created with the node, so every clone of that
   parse node (plans, the `stmt_pool` copy, `CompiledFunction::param_defs`, a closure's shared
   signature) observes one slot. The compiler fills it when it compiles the routine or closure
   that owns the signature, with a standalone, env-resolved chunk compiler rooted at the
   declaration (the ADR-0019 C5 `compile_decl_expr` shape). The binder runs the chunk; it never
   compiles.
3. **An unfilled slot is a correctness fallback, not an alternative mechanism.** A `ParamDef`
   synthesized at runtime (introspection, `.assuming`, RakuAST lowering after compile) has no
   chunk, and binds through the old AST path. Any code that rewrites a `ParamDef`'s `where`,
   default or shape expression must reset its `ParamCode`, since the slot describes the
   expressions the node was compiled with.
4. The remaining per-call AST compile sites are tracked as issues, one per family, and retired
   under this ADR: WhateverCode subscripts
   ([#10118](https://github.com/tokuhirom/mutsu/issues/10118)), sequence endpoints
   ([#10119](https://github.com/tokuhirom/mutsu/issues/10119)), `.subst` closure replacements
   ([#10120](https://github.com/tokuhirom/mutsu/issues/10120)), and the regex
   `<{ }>`/`** {n}`/`:my` interpolations
   ([#10121](https://github.com/tokuhirom/mutsu/issues/10121)). Each was confirmed with the
   `Compiler::compile` breakpoint count above; sites a code reading suggested but the count did
   not confirm (`cas` with a computed operand, `is rw` method-lvalue assignment, user-infix
   fallback, a typed sequence generator) are not filed until a probe shows them hot.

## Consequences

- Slice 1 (#10107) makes every binder path — ordinary routines, methods, closures, multi
  candidate selection, sub-signatures and shape constraints — run precompiled chunks for
  `where` clauses, non-literal defaults and shape dimensions.
- The chunk is compiled once in its declaration's context instead of in whichever context the
  first call happened to see, so `$?PACKAGE`-sensitive and package-qualified names in a
  signature expression resolve against the declaring package on every path.
- `ParamDef`'s `Hash` ignores `ParamCode`, so routine fingerprints are unchanged.

## Rejected alternatives

- **A content-keyed runtime cache** (hash the `Expr` + ambient context per call). It removes the
  compile but not the AST dependency, costs a hash and a context key on every call, and repeats
  the context-key drift risk `carrier_compile_ctx_key` documents.
- **Per-call-site identity keyed by the `Expr`'s address.** A clone of the `ParamDef` changes the
  address, and a freed node's address can be reused — a wrong-hit hazard.
- **Filling chunks at every plan construction site.** Signatures reach the runtime through a
  dozen copies (sub/method plans, `stmt_pool`, `CompiledFunction`, closure signatures, role
  bodies); a slot shared by all clones of the parse node needs one fill point per compiled
  routine instead.

## Implementation status

- Slice 1 — signature expressions (`where`, default, shape): implemented (#10107). Beyond the
  compile removal, the slice routes a `where`/subset callable through `vm_call_on_value`
  instead of `call_sub_value`'s env carrier, and answers a one-argument WhateverCode predicate
  (`where * < 100`, `subset … where * < 100`) by running its body with `$_` bound, building no
  closure. The #10107 repro went from 178.7k to 56.1k instructions per call; the remainder is
  the value-dependent multi-dispatch machinery itself (candidate gather/rank, `type_matches_value`,
  the env snapshot around each `where`) and stays open under #10107.
- Later slices: #10118, #10119, #10120, #10121 (Decision 4).
