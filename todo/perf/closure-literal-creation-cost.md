# Creating a closure literal still costs O(enclosing env)

Found while decomposing `bench-ctor` (2026-08-29). It is not a bench-ctor
problem — it is paid by every `.map({...})` / `.grep({...})` / callback literal
created in a loop, anywhere in the language.

## Measurement

`tmp/lambda-create.raku` builds a `* + 1` WhateverCode 200000 times without
calling it; `tmp/lambda-none.raku` is the same loop with the creation removed
(release, `taskset -c 2`, best of 5):

| | mutsu (before) | mutsu (after part A) | raku |
|---|---|---|---|
| control loop | 0.1323 | 0.1357 | 0.1518 |
| + 200000 closure creations | 0.6259 | 0.5309 | 0.1847 |
| **per creation** | **2.47us** | **1.97us** | **0.16us** |

Part A (below) landed the fixed-cost reductions: **−20% per creation**, still
**~12x rakudo**.

## The remaining cost scales with the enclosing env, not with the closure

Adding 30 lexicals whose names do not start with a lowercase letter
(`my $Aa1 … my $Ad0`) to the same file — none of them referenced by the closure —
adds ~29ns per creation per entry (0.531s → 0.706s over 200000 creations).

`capture_closure_env` (`src/vm/vm_register_ops.rs`) captures, in addition to the
closure's actual free variables, **every env key for which
`env::is_plain_user_lexical` is false** — i.e. every name that does not start
(after its sigil) with a lowercase ASCII letter. That rule is deliberately
conservative because mutsu stores scalars sigil-less, so a user's `my $Foo` and a
bare type name `Foo` are the *same* env key shape and cannot be told apart; the
kept set therefore has to include dynamics (`$*x`), magic vars (`$_`, `$!`,
`$/`, capture digits), `self`, `?CLASS`, all `__mutsu_*` metadata — and, as
collateral, every uppercase-initial user lexical in scope.

`Env::filtered_flat` then builds a brand-new `SymMap` and inserts each kept
entry, which is where the profile still lands after part A: `reserve_rehash`
6.4%, the filter closure 3.8%, `HashMap::insert` 2.5%, `filtered_flat::collect`
1.8% — ~14% of the run building that map, plus ~13% in `gc_op` and ~3% in
`Gc::drop` refcounting the captured values.

## Part A — DONE (2026-08-29)

Fixed per-creation costs, all contained and semantics-free:

- `capture_closure_env` rebuilt a `HashSet<Symbol>` of the free vars and a
  `HashSet<&str>` of the chunk's own locals on **every** creation, although both
  are pure functions of the `CompiledCode`. Now built once per chunk
  (`CompiledCode::capture_free_var_set` / `capture_local_set`), and the locals
  test compares `Symbol`s instead of hashing the key's string.
- `env.remove("__mutsu_return_type")` / `env.insert("__mutsu_callable_type", …)`
  and `Symbol::intern("__mutsu_callable_type")` re-interned a fixed literal per
  creation — the `String`-keyed `Env::insert` also allocated the literal and
  re-scanned it in `note_env_key`. Now pre-interned via `symbol::well_known`.
- `Symbol::intern("")` for the anonymous closure name — same treatment.

## Part B — OPEN: the O(kept-env) capture

This is the bigger lever and the riskier one. The kept set is over-broad by
construction, and narrowing it means deciding that free-variable analysis is
authoritative for some class of names it currently is not trusted for. That is
exactly the "incomplete static analysis turns into a *flaky* failure" shape
CLAUDE.md warns about (the `roles-6e.t` precedent), so it needs a real design
pass — probably an ADR — not a drive-by: which names are genuinely read by
*runtime* by-name mechanisms (dynamics, magic vars, `self`/`?CLASS`,
`__mutsu_*` metadata, `$OUTER::`, `reflective_name_access_possible()`) versus
which are only ever read through a compiled `GetLocal`/`GetGlobal` the free-var
pass already sees.

A cheaper alternative worth costing first: keep the capture semantics but stop
rebuilding the map — e.g. share the system-name portion through the `Env` parent
chain instead of copying it, if snapshot semantics allow.

## Part C — DONE (2026-08-29): the body is shared, not deep-cloned

`SubData::body` was a `Vec<Stmt>`, so every creation deep-cloned the block's AST.
Creation was **O(body size)**: 8.2us for a 29-statement block against ~2us for a
one-statement one. It is now `Arc<Vec<Stmt>>`, built once per `stmt_pool` slot by
`CompiledCode::closure_body_arc`, and the two are the same number. The 29-statement
micro dropped 70%; `bench-ctor` −11.2%, `word-count` −5.2%.

## Still open beyond Part B

- `params.clone()` / `param_defs.clone()` per creation (small next to the body,
  but the same shape — both are pool-owned and immutable).
- `Symbol::intern(&self.lexical_closure_package())` allocates a `String` per
  creation just to intern it.
- ~~`exec_make_gather_op` runs `Compiler::new().compile(...)` on every `gather`
  block creation~~ — **DONE (2026-08-29)**. It was the identical
  per-creation-compile shape `eval_map_over_items_rw` had (#7109), and it was
  hot: `add_constant` grew by 3 per gather creation (60013 over a 20000-iteration
  loop), and adding 20 statements to the body took that loop from 0.11s to 0.81s
  while rakudo went 0.23s to 0.27s. `Interpreter::gather_compile_cache` keys the
  compile on the body's analysis chunk: `add_constant` 60013 → 16, the small-body
  loop −34%, the large-body loop −71% (0.80s → 0.23s, now faster than rakudo's
  0.27s).

## Repro

Recreate `tmp/lambda-create.raku`, `tmp/lambda-none.raku` and a variant with 30
extra uppercase-initial lexicals (`tmp/` is gitignored). Profile with
`perf record -e cpu_core/cycles/`; the `reserve_rehash` caller was located with a
`rust-gdb -batch` breakpoint on `hashbrown::raw::RawTable<T,A>::reserve_rehash`
(backtrace: `HashMap::insert` -> `Env::filtered_flat::collect` ->
`Env::filtered_flat` -> `capture_closure_env` -> `exec_make_lambda_op`).

**Beware `benchmarks/bench-startup.raku` when A/B-ing here**: at ~4.5ms it is so
short that the same binary measures 0.0045s and 0.0086s in consecutive rounds. A
large percentage swing on that one row is noise, not a regression.
