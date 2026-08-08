# ADR-0019 D2c-4: attribute default/where-constraint expressions become precompiled chunks

`CompiledAttrDecl::default`/`where_constraint` are now `Option<DeclTraitArg>` instead of
a raw `Option<Expr>`, precompiled to `Literal`/`Compiled` bytecode chunks at declaration-plan
lowering time — the same treatment `is_default` already got in D2c-1, and matching
`ClassAttributeDef`'s own field type since D2c-2. Both class and role `attr_decls` gain this;
roles previously never compiled any of their trait arguments (D2b-2 deliberately scoped that
out), so this is the first time a role attribute's `default`/`where_constraint`/`is default(...)`
skip the on-demand-compile-per-registration path.

Two consumers used to read the raw `Expr` off a `DeclTraitArg::as_expr()` call, which panics on
a `Compiled` chunk. Both needed fixing in the same slice to keep the panic-free invariant:

- The shaped-`@`-attribute pattern match (`has @.a[2]` recognizing the compiler-generated
  `Array.new(:shape(...))` default) moved to a precomputed `declared_shape: Option<Vec<usize>>`
  field on both `CompiledAttrDecl` and `ClassAttributeDef`, computed once from the raw `Expr`
  before it is discarded — the same precompute-a-pure-syntactic-fact pattern D2a used for
  attribute name pre-scans. The two small functions that recognize the pattern moved from
  `runtime/methods_signature.rs` (an `Interpreter` method with a single caller) to free functions
  in `opcode.rs`, next to `CompiledAttrDecl` itself.
- The `.^attributes` introspection closure (`Attribute.build`, which wraps a non-literal default
  in a lazy `Code` object) now branches on `DeclTraitArg::Compiled` and builds its `SubData`
  directly from the chunk's `compiled_code`/`compiled_fns` fields instead of reconstructing an AST
  body via `.as_expr()`.

That second fix surfaced two real, narrow gaps in the surrounding machinery, both fixed rather
than routed around:

1. **`run_decl_expr` (the `Compiled`-chunk execution entry) was missing the topic (`$_`)
   save/restore `vm_eval_block_value` already carries for the `Ast` path** (added in #6071 for
   `class S { has Bool $.b }; $_ = 'x'; S.new`, which left `$_` holding `Bool`). An attribute with
   no explicit default synthesizes an implicit "unset typed attribute" default expression, and once
   this slice made that expression compile to a `Compiled` chunk, its evaluation started escaping
   through the unpatched `run_decl_expr` path — `t/decl-time-value-block-keeps-the-topic.t` caught
   it in `make test` (the targeted roast sweep for this slice didn't happen to exercise it). Fixed
   by giving `run_decl_expr` the identical save/restore, factored into a shared `run_decl_code`
   helper both entry points now call.
2. **A `Compiled`-chunk `SubData` returned `Nil` when actually invoked as a `Code` object**
   (`roast/S12-introspection/attributes.t`'s `.build().(C, $_)`). `Compiler::compile_decl_expr`
   produces a standalone "value block" — no signature, no `Return`-based call ABI — meant only for
   direct execution via `run_nested`. Installing that bytecode as `SubData.compiled_code` made the
   general call path (`vm_call_on_value`) try to invoke it through `call_compiled_closure`, which
   expects the closure/routine calling convention and silently produced `Nil`. The obvious signal
   to distinguish this shape from an ordinary closure — `body.is_empty()` — turned out to be
   unsafe: an ordinary `sub (Int $x) {}` also has an empty body and must still go through
   `call_compiled_closure` to type-check its argument, so trusting that signal regressed
   `t/exception-types.t`'s binding-error tests. Fixed with an explicit `SubData::is_decl_expr_thunk`
   marker (touching every `SubData` construction site, `false` everywhere except this one) and a
   `vm_call_on_value` arm that routes a marked thunk through `run_decl_code` instead, ignoring call
   args exactly as the on-demand-compiled AST-body `Sub` it replaces already did.

Verified via the full `t/` suite (27,949 tests), every whitelisted `S12-attributes`/`S14-roles`/
`S09-typed-arrays`/`S12-construction`/`S12-meta`/`S06-signature` roast file (102 files) with the
release binary, and a manual raku-vs-mutsu comparison of a construction-in-a-loop covering class
and role `default`/`where`/`is default` attributes.
