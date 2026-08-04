# ANALYSIS.md rev12 removes completed work from the active roadmap

The architecture review was refreshed against `435de2d3e`, 302 commits after rev11.
It now records ADR-0013 as closed and gated, ADR-0016's materialization counter,
ADR-0018's completion, and ADR-0019's 14/51-slice progress. Live source-size and
hygiene metrics were re-measured.

The roadmap now lists only current work. ADR-0019 remains first because its transitional
plans, mirrors, caches, and resolvers are already live on `main`. Exception role/type
registration follows, aligned with ADR-0019's TypeId/MRO work. The missing batteries-policy
and worker-pool ADRs are separated from implementation work, while the evidence-starved
Proc::Async crash is treated as a conditional P0 when a new crash report makes it actionable.
