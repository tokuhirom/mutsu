# The routine identity fingerprint lives on the routine, not in a side cache

Groundwork for ADR-0019 C6 (removing `CompiledSubDeclPlan::legacy_body`), which
cannot happen while `FunctionDef.body` has dozens of readers. This retires one
whole category of them.

A routine's *structural identity* — the hash of its signature and body — is what
multi-candidate identity, `state`-variable scoping, wrap chains, `MAIN` candidate
dedup, and redeclaration checks all key on. It was computed by
`function_body_fingerprint(&def.params, &def.param_defs, &def.body)`, which
Debug-renders the entire body AST through a hasher. Eight call sites did that
directly, several of them per dispatch, and the cost was well known: a side cache
(`func_def_fp_cache`, an `FxHashMap` keyed on the def's `Arc` pointer, holding a
clone of every def it had ever seen) existed for no other reason than to avoid it
on the multi-redispatch path.

The fingerprint is now `FunctionDef::body_fingerprint()`, memoized inline in a
`OnceLock<u64>` on the def itself. It is computed at most once per def, needs no
map probe, cannot miss, and cannot go stale — a clone carries the memo because it
carries the same body, and the single site that rewrites a body in place (the
`proto` dispatch rewrite in `vm_call_func_ops`) drops it explicitly via
`invalidate_body_fingerprint()`. `func_def_fp_cache` and `func_def_fingerprint`
are deleted; the field is `#[serde(skip)]`, so a deserialized def simply
recomputes on first use.

Net effect on ADR-0019: `FunctionDef.body` readers drop from 58 to 50, and every
remaining one is a read that genuinely wants the AST rather than an identity hash.
The `MethodDef` twin (`method_body_fp_cache`) is deliberately left alone — it is
keyed on the body `Arc`, which is *shared across* `MethodDef` clones, so a
per-instance memo there would recompute more often, not less. It belongs with the
phase-D method work.
