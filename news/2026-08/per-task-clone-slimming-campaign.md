# Per-task clone slimming: spawn overhead now below raku (campaign complete)

The `docs/per-task-clone-slimming.md` campaign — the companion lever to the
ADR-0020 worker pool — landed all its slices on 2026-08-05:

- **Slice 0** (#5928): `SharedStore` lineage maps on `FxHashMap` instead of
  SipHash.
- **Slice 1** (#5929): Registry copy-on-write — `Arc<RwLock<Arc<Registry>>>`
  with `Arc::make_mut` on first write, replacing the eager ~40-map deep clone
  per spawn (and retiring the regex-eval generation-cached snapshot).
- **Slice 2** (#5930): single-pass env iteration in
  `clone_for_thread_excluding` (lineage seeding + IO-handle scan merged).
- **Slice 3** (#5931): process-constant IO env singletons (`$*DISTRO`,
  `$*PERL`/`$*RAKU`, `$*VM`, `$*KERNEL`, executable/tmpdir/home strings)
  cached in `OnceLock`s.
- **Slice 4** (#5932): `instance_type_metadata` copy-on-write, same pattern
  as slice 1.
- **Slice 5 step A** (#5933): `spawn_seed_keys`/`spawn_seed_inserts` counters.
  They showed the seeding walk is ~99.98% redundant re-walk on a same-scope
  spawn loop (120000 keys walked, 23 inserted) — and a measurement hack then
  bounded the win of skipping it at **zero** (release 0.70–0.74s vs
  0.71–0.73s), so the review-gated **step B generation skip was retired
  unimplemented**: after slice 0 the walk costs single-digit milliseconds
  total, and the skip machinery would have been pure flake-risk surface.
- **Slice 6** (#5934): `start` blocks now inherit the spawning scope's
  dynamic IO vars — a Raku-compat fix (a redirected `my $*OUT = ...` capture
  object is visible inside `start`, pin `t/start-inherits-dynamic-out.t`)
  that also removed four `create_handle` calls and eight env inserts per
  task, the single largest perf win of the campaign.

Numbers (local A/B, release build, median of 5 — this spawn-shape bench is
not in the bench-CI suite):

- Bench `for ^2000 { await map -> $k { start { $k * 2 } }, 1, 2 }` (4000
  tasks): **1.66s → 0.19s**, past the plan's < ~1.0s exit criterion and below
  raku's 0.33s on the same machine. Pool reuse intact
  (`tasks=4000 warm_reuses=3997`), `registry_cow_clones=0`.
- `Digest::RIPEMD` (`t/ripemd.t`, the owner ticket's target): ~513s →
  **295.3s** (9/9 pass), still over the 120s batteries-gate budget. The
  remaining gap has a measured single cause — the 80-round compression loop
  never enters the JIT (`jit: compiles=0`, every chunk bails on
  `BitShiftLeft`) — handed off to
  `todo/tickets/jit-bitwise-tier-a-coverage.md`.

Incidental finding recorded in the plan doc: over-approximating the per-spawn
`referenced_handle_ids` set to "all handles" makes the bench 14× slower — the
referenced-only filter on handle cloning is load-bearing.
