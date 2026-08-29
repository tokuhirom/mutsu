# Closure creation stopped rebuilding two per-chunk sets on every literal

Creating a closure literal is something mutsu does once per `.map({...})` call,
once per callback, once per `TWEAK` that contains a block — i.e. per *iteration*
of any loop that mentions one. A micro that builds a `* + 1` WhateverCode 200000
times without calling it (against a control loop with the creation removed,
release, `taskset -c 2`, best of 5) measured:

| | mutsu | raku |
|---|---|---|
| control loop | 0.1323 | 0.1518 |
| + 200000 creations | 0.6259 | 0.1847 |
| **per creation** | **2.47us** | **0.16us** |

~15x. This change takes the fixed part of that down by 20%, to 1.97us. The rest
is the O(enclosing-env) capture, which needs a design pass and is tracked in
`todo/perf/closure-literal-creation-cost.md` as Part B.

## What was being redone per creation

`capture_closure_env` (`src/vm/vm_register_ops.rs`) decides what a new closure
inherits from the frame creating it, using two membership sets:

- the closure's free variables, and
- the closure's *own* locals/parameters, which must NOT inherit a same-named
  enclosing binding (a WhateverCode's `_` param must not swallow the creating
  frame's `for`/map topic and leak it back on return).

Both are pure functions of the block's `CompiledCode`, and both were re-collected
into fresh `HashSet`s on **every single creation** — two allocations plus their
fills, per closure. They are now built once per chunk
(`CompiledCode::capture_free_var_set` / `capture_local_set`, behind the same
`OnceLock` pattern `const_syms` and `local_attr_keys` already use). The
own-locals test also compares interned `Symbol`s instead of `&str`: `locals_sym`
is the interned twin of `locals`, so membership is identical without hashing the
key's string on every probe.

Three fixed literals were also re-interned per creation: the anonymous closure's
empty name, `__mutsu_return_type` (removed from the capture, then re-inserted
when the block declares a return type) and `__mutsu_callable_type` (the
WhateverCode marker). The `String`-keyed `Env::insert`/`Env::remove` twins
allocated the literal, hashed it in the intern memo and re-scanned it in
`note_env_key` — which sets no flag for either name. They now go through
`symbol::well_known`, a small home for names the runtime interns on a
per-operation hot path. `sip::Hasher::write` disappeared from the profile
entirely as a result.

## Effect

Interleaved A/B of the two release binaries, `taskset -c 2`, best of 5: the
closure micro **−16%**, `bench-array` **−9.8%**, everything else in
`benchmarks/` within noise. (`bench-startup` is not a usable A/B row here: at
~4.5ms the *same* binary measures 0.0045s and 0.0086s in consecutive rounds.)

Pin: `t/closure-capture-symbol-sets.t` — 17 assertions over exactly the
semantics those two sets encode: per-iteration capture of a loop variable, a
capture tracking a later mutation, a WhateverCode's `_` not taking or leaking the
caller's topic, a block-local declaration shadowing without writing through,
uppercase-initial lexicals / dynamics / type names / `self` staying reachable, a
closure created inside a closure, and a bare block still reporting `Block` (i.e.
the WhateverCode marker not being inherited). Green under real `raku`.
