# ADR-0019 D6-3d: wire `run_class_body` to the compiled `body_plan`, instrument-gated

Following D6-3a-c's additive `body_plan` (a typed, position-aligned mirror of a
class body's flattened statement list, with a compiled `CompiledDeclExpr` chunk
for every non-token/rule arm), this slice cuts the driver over: `run_class_body`
now zips `body_plan` against the existing flattened `legacy_body` walk (both are
built by the identical flatten+nested-has-append transform, so their order and
length agree by construction), and its three small-statement arms
(`class_body_other_stmt`/`ClassSub`, `class_body_code_alias`,
`run_class_body_leave_phasers`) can run a statement's precompiled chunk instead
of the registration-time `run_block_raw` on-the-fly compile.

A new `Interpreter::run_compiled_block_raw` carries `run_block_raw`'s
post-compile half (`run_nested` plus the `free_var_writes` → pending-writeback
drain) so both the on-the-fly and precompiled paths share the exact same
execution/writeback semantics.

The cutover is gated behind `MUTSU_DROP_LEGACY_CLASS_BODY=1` (the
`MUTSU_DROP_LEGACY_BODY`/C6e-3a precedent from the sub side) and unset by
default, so this slice ships with zero behavior change — the instrument exists
to validate the chunk path exhaustively before a later slice flips the default.
`ProtoMethod`'s chunk stays unused: `class_body_proto_method_decl` never
actually executed the raw statement (it only clones `proto_body`/`param_defs`
off the AST into a `FunctionDef`), so there is nothing for its chunk to
replace yet.

Wiring the instrument surfaced two real bugs in the previously-dead D6-3a-c
chunks, invisible until something finally consumed them:

1. **`LeavePhaser`'s chunk compiled to a silent no-op.** D6-3c compiled it
   from the *wrapping* `Stmt::Phaser{kind: Leave, ..}` statement, but an
   un-lowered `PhaserKind::Leave` compiles to nothing on its own (LEAVE is
   normally driven by the enclosing `BlockScope` registering a callback, not
   direct statement compilation) — while the runtime actually runs the
   phaser's *inner* `body`. Fixed by generalizing the chunk compile into
   `Compiler::compile_decl_stmts_chunk_in_package` (accepting `&[Stmt]`) and
   feeding it the phaser's own inner body.
2. **Every D6-3b/c chunk qualified bare variable/sub names against the wrong
   package.** Bare-name package qualification is baked in at compile time
   from the compiler's `current_package`, but the child compiler used for
   each chunk inherited the *outer* (enclosing) compiler's ambient package
   instead of the declaring class's own name. A top-level
   `no strict; class Foo { $foo = 42; }` wrote an unqualified global instead
   of `Foo::foo` under the forced instrument — caught by the pre-existing
   `t/strict-use-and-eval.t`. Fixed by threading the same `package_name`
   already computed for main-pass method-body compilation into
   `compile_class_body_plan`, falling back to `chunk: None` (the
   `run_block_raw` path) for a computed class name/hoisted shell, exactly
   like the method-body precedent.

Both fixes are pinned by new compiler unit tests
(`class_declarations_leave_phaser_chunk_compiles_inner_body`,
`class_declarations_other_chunk_qualifies_against_declaring_class`).

Verified with `MUTSU_DROP_LEGACY_CLASS_BODY=1` forced: the full `t/` suite
(28,023 tests), the `S12-class`/`S12-construction`/`S14-roles`/`S05-grammar`
roast files (1,042 tests, same pre-existing `open_closed.t` failure as
unforced), and `scripts/battery-testsuite.sh` (158/164 files pass, 2 excluded
— byte-identical PASS/FAIL output to the unforced baseline).

Next: a later slice flips the default once more validation has accumulated,
then D6-4 drops `CompiledClassDeclPlan::legacy_body` (modulo the token/rule
rump).
