# `ANALYSIS.md` is archived as a 2026-09 snapshot

The top-level `ANALYSIS.md` was an architecture and soundness review verified
against the tree on 2026-09-12. It was never refreshed after that, and many of
its findings have since been fixed, so keeping it at the top level next to
`README.md` and `PLAN.md` presented a stale picture as a current one.

It now lives at `docs/archive/analysis-2026-09.md`, under a header that marks
it as a frozen historical snapshot, with its relative links fixed for the new
location. Every reference to it outside `news/` (`PLAN.md`, ADR-0001, 0011,
0018 and 0019, the GC, dual-store, decoupling and lexical-scope design docs,
and source and test comments that cited one of its sections) now names the
archived file. Current architecture is described in `docs/architecture.md`.
