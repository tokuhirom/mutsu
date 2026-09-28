# A burst of blocked `start` workers completes under `ulimit -v`

ADR-0123 bounded the address space that `start` worker stacks may take when
the process runs under `RLIMIT_AS`, as the ecosystem sandbox does with its
6 GB `ulimit -v`. It still failed on one shape: many `start` blocks that all
**block** at once. The JobQueue distribution's `t/01-queue.rakutest` parks 32
callers on one `await $start`, and under the sandbox it died partway with
`memory allocation of 16912 bytes failed`.

Two things spent the address space the heap needs:

- **Full-size stacks past the budget.** Each blocked worker forces the pool to
  grow, and a thread that *has* to exist was allowed past the stack budget. But
  it went past it at the full 256 MiB tier, retrying the smaller tiers only once
  the OS itself refused, and one measured run held 18 full stacks (4.6 GB
  against a 3 GB budget). Such a thread now takes the largest smaller tier that
  still fits the budget, and only past it with the smallest, 32 MiB tier.
- **glibc's per-thread malloc arenas.** Each reserves 64 MiB, and glibc creates
  up to `8 × cores` of them as threads contend. Under an address-space limit,
  mutsu now caps them at startup (`M_ARENA_MAX`, about a sixteenth of the
  limit, never below two). A `MALLOC_ARENA_MAX` the user sets still wins, and
  nothing changes without a limit.

`t/concurrency/thread-lock/thread-pool-stack-budget.t` now also parks 48
workers under a 6 GB limit and requires every one to complete. Before the fix
it lost about a third of them. The details are recorded as ADR-0123 §5.
