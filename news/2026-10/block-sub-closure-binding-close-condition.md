# A closure's call of a block-declared sub is back within 5% of its old cost

ADR-12529 phase 2 slice 4 made a closure call a block-declared routine through the code object of
the activation that declared it. That cost `bench-blocksub` up to 30% (#13049); #13446 fixed the
per-call half. This change removes most of the per-activation half, which is paid when the closure
is created:

- The activation's code object is built from the routine the activation just registered, read from
  its registry key, instead of through the generic `&name` resolver (pseudo-package, alias, multi and
  proto probes).
- The code object's data is built once, and the activation capture rewrites it in place instead of
  copying it a second and third time.
- A `RegisterDecl` no longer builds its inline-package marker key while no such marker exists in the
  process, and no longer rewrites an unchanged `mainline_lexical_subs` entry or hashes the sub's free
  variables.

Release build, JIT off, `scripts/bench-det.sh` Ir: `bench-blocksub` 467.8M to 442.8M (close
condition: at most 447.8M), `bench-blocksub2` 464.3M to 437.7M.
