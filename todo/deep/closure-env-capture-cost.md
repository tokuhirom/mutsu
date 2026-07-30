# Closure env capture materializes the world on every lambda creation

Found while profiling bench-ctor (2026-07-30, frame-pointer profile on the
40k-iteration variant): `exec_make_lambda_op` is ~11% inclusive and
`capture_closure_env` ~8-10% of total runtime — for a benchmark whose only
lambda is the constant WhateverCode `*.flat` inside `Dist.TWEAK`
(`@!resources = @!resources.map(*.flat)`), created fresh on every
construction. The same cost is paid by every `.map({...})` / `.grep({...})`
in every hot loop (grammar actions, YAMLish parsing), so this is a general
throughput tax, not a bench-ctor quirk.

## Root cause

`capture_closure_env` (src/vm/vm_register_ops.rs) builds the closure's env as
a **flat materialized map**: it iterates the entire flattened creating-frame
env and filter-copies every entry that is a free variable or a "system name"
(everything `is_plain_user_lexical` rejects — all uppercase type names, all
`&`-subs, `?`-vars, dynamics, `__mutsu_*` fixtures, `self`, `_`, `!`). In a
program with N global symbols this is O(N) hash inserts + Value clones per
closure creation, even for a closure with zero free variables: the profile
shows the cost split between the filter walk (`Symbol::with_str` +
`hash_one`), `HashMap::insert`/`reserve_rehash`, and
`Env::from(HashMap)`.

The filter exists for lexical hygiene (a closure must not see non-captured
user lexicals; a WhateverCode must not inherit the enclosing topic `_` and
leak it back), and the flat result exists because the closure's env is its
*only* name source at run time — the parent chain is not kept.

## Why it is not a quick fix

- Keeping the parent chain by `Arc` (a `scoped_child` capture) would be O(1),
  but exposes non-captured user lexicals to any by-name lookup path and
  changes the write-back/merge topology of closure exits; `SubData.env` flows
  through persisted closure state (`closure_env_overrides`), `OUTER::`
  snapshots, and thread spawns, and some consumers may assume flatness.
- Caching the captured env per creation site is unsound as-is: the kept set
  includes per-frame values (`self`, `?CLASS`, `_`, `!`) and the global tier
  mutates when subs/classes are defined at runtime.
- The sound shape is probably a **two-tier capture**: a shared, Arc'd
  "system/global tier" snapshot (invalidated by a global-symbol epoch counter
  bumped on non-plain-lexical inserts) + a tiny per-creation overlay (free
  vars from slots, `__mutsu_outer::*`, frame fixtures). That needs an env
  audit and belongs with the Slice F env work (PLAN §6) — design first, not a
  drive-by patch.

## Measurement notes

Repro: `benchmarks/bench-ctor.raku` with iterations raised to 40k;
`perf record -g --call-graph fp` on a `force-frame-pointers` profiling build
(DWARF unwinding of the default build is impractically slow in `perf
report`). See `todo/tickets/bench-ctor-construction-parity.md` for the
surrounding campaign; this file is the S-side deep item referenced there.
