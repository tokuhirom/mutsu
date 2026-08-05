# ADR-0020 proposed: shared worker pool — elastic growth, blocking `await`

Wrote the Proposed ADR the worker-pool ledger item called for
([docs/adr/0020-shared-worker-pool.md](../../docs/adr/0020-shared-worker-pool.md)),
retiring `todo/deep/shared-worker-pool-adr.md`. The central fork — what `await` does on a
pooled worker, given mutsu has no continuations — is decided in favour of an **elastic
pool that keeps `await` blocking** and grows on starvation, Rakudo-supervisor-style;
continuation-ifying `await` is recorded as the future ADR that would supersede the
elasticity, not as the opening move.

The groundwork was re-measured on main `a85d464a3` (the 2026-07-17 numbers reproduced:
ripemd shape ~17× raku, 50 idle `cue(:every)` still own 52 threads / 16.7 GB VmSize), and
extended with the decomposition the decision needed:

- **Raw OS thread machinery is only ~10% of per-`start` cost.** A Rust microbench running
  the exact thread-per-task shape (4000 threads, 256 MiB stacks, joined in pairs) costs
  ~155 ms where mutsu spends 1.50 s.
- **The per-task `clone_for_thread` is the dominant, env-proportional share**: adding 200
  idle lexicals to the spawning scope makes the same loop 80% slower, and the flat perf
  profile is malloc/hash/hashmap traffic under `clone_for_thread_excluding`.

So the ADR is honest about scope: the pool fixes the resource pathologies (idle cues owning
threads, spawn churn defeating STW bookkeeping, ADR-0008's `cue(:every)`-onto-the-timer
follow-up) and is the substrate a warm-clone optimization needs — but whitelisting Digest's
`t/ripemd.t` (the motivating battery gap, `todo/tickets/digest-ripemd-start-per-block-overhead.md`)
also needs per-task clone slimming, which stays in that ticket.
