# Bare-block compilation is still split between `Stmt::Block` and `compile_do_block_expr`

## Background

This is the residue of
`todo/deep/unify-statement-expression-control-construct-compilation.md`, which
has otherwise been resolved (see
`news/2026-09/unify-statement-expression-control-construct-compilation.md`).
`for` and `if`/`elsif` chains now have one lowering each, shared by both source
positions (`src/compiler/control_for.rs`, `src/compiler/control_if.rs`), and
`while` / C-style `loop` / `lazy for` in expression position were already thin
`gather`-desugaring wrappers over the statement path.

The bare `{ ... }` block is the one construct still compiled by two independent
passes.

## Root cause

- **Statement position**: the `Stmt::Block` arm of `Compiler::compile_stmt`
  (`src/compiler/stmt.rs`).
- **Expression position**: `compile_do_block_expr` and
  `compile_do_block_expr_scoped` (`src/compiler/helpers_do_expr.rs`).

Both decide the same things — whether the block declares routines, whether it
imports (`PushImportScope`/`PopImportScope`), whether it needs a per-execution
`ResetStateLocals`, whether a `CATCH`/`CONTROL` in it makes it an implicit `try`,
whether ENTER/LEAVE/KEEP/UNDO phasers make it a real phaser block scope, and how
placeholder variables declared inside it are attributed — and each writes its own
answer. They then emit *different* opcodes for the scope itself (`BlockScope` vs
`DoBlockExpr`), which is why this did not fall out of the `for`/`if`
unification: those two shared a single opcode and a single skeleton, and the
block pair shares neither.

## Why it is large

Unlike `for` and `if`, this is not "the same code with a `collect` flag". A
correct unification has to decide first whether `BlockScope` and `DoBlockExpr`
should stay two opcodes at all, and if they should, what exactly each guarantees
about the value stack, the env, the import scope and the phaser queues — then
express the statement form as the value form with the result discarded (or the
other way round). Both arms are load-bearing for a very large amount of
whitelisted roast code, and the placeholder-attribution rules in particular
(ADR-0048 D3/D6, `is_construct_body_block`, `synthetic_block_body`) are subtle
and were arrived at incrementally.

An ADR is probably warranted before any code lands: the opcode question is
exactly the kind of costly-to-reverse decision `docs/adr/README.md` asks to be
recorded.

## Evidence

```
grep -n "fn compile_do_block_expr" src/compiler/helpers_do_expr.rs
grep -n "Stmt::Block(stmts) =>" src/compiler/stmt.rs
```
