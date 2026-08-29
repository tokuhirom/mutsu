# A closure's body is shared, not deep-cloned on every creation

`SubData::body` was a `Vec<Stmt>`, and every closure creation deep-cloned the
block's whole AST into it. Closure creation runs once per `.map({...})` **call**,
once per callback, once per `TWEAK` containing a block — so the cost of writing a
`.map` with a real body was paid, in full, on every iteration of the loop around
it.

A micro that builds a pointy block 200000 times without ever calling it (release,
`taskset -c 2`, best of 5) shows it directly:

| block body | before | after |
|---|---|---|
| 1 statement | 0.5701 | 0.5153 |
| 29 statements | **1.7194** | **0.5146** |

Creation was **O(body size)** — 8.2us per creation for the 29-statement block,
against ~2us for a one-statement one. It is now flat: the two rows are the same
number, because the body is no longer copied at all.

## The change

`Stmt::SubDecl` / `Stmt::Block` bodies live in the chunk's `stmt_pool` and are
never mutated through the `SubData` that carries them — the `Arc<Vec<Stmt>>` type
change proves that, since `Arc` has no `DerefMut` and the build has no mutation
site to fix. So every closure created from a given pool slot shares one body:
`CompiledCode::closure_body_arc(idx)` builds the `Arc` once per slot (the same
`OnceLock` side-table pattern `const_syms` and `local_attr_keys` already use) and
each later creation is an `Arc` bump.

All four closure-creation opcodes go through it — `MakeLambda`,
`MakeBlockClosure`, `MakeAnonSub` and `MakeAnonSubParams`. (Only some of them had
picked up the previous round's symbol-interning fix; they are consistent now,
which is worth stating: the bug this whole line of work started from was one
map/grep loop out of four missing a cache its siblings had.)

The `Value::make_sub*` constructors take `impl Into<Arc<Vec<Stmt>>>`, so a caller
that builds a body at runtime still passes a `Vec` and a caller that already
holds a shared body passes it through without copying.

## Effect

Interleaved A/B of the two release binaries, `taskset -c 2`, best of 5:

- the 29-statement closure micro **−70%**
- `bench-ctor` **−11.2%** (its `TWEAK` creates a `*.flat` closure per construction)
- `word-count` **−5.2%**, the flat closure micro **−4.0%**
- every other benchmark within ±2%

(`bench-startup` is not a usable A/B row: at ~4.5ms the same binary measures
0.0043s and 0.0087s in consecutive rounds.)

## Noted while here, not fixed

`exec_make_gather_op` runs `Compiler::new().compile(...)` on **every** `gather`
block creation — the same per-creation-compile shape that
`eval_map_over_items_rw` had. Recorded in
`todo/perf/closure-literal-creation-cost.md`.
