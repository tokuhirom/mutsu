# PERFORMANCE.md retired; its living rules moved to docs/benchmarks.md

`PERFORMANCE.md` had become a history book: its "Current Status" table was frozen at main
`c8955d2e` (2026-07-13), and most of the rest narrated bottlenecks that were fixed months ago
(the fib `?LINE` overlay regression, the method-dispatch AST fingerprint, the bench-class topic
writeback) and the JIT phases J1-J5, all of which are recorded in `news/` and the ADRs. A stale
ratio table next to "source of truth" wording invites quoting numbers that no longer hold.

Two parts were still rules nothing else wrote down, and they now live in
[`docs/benchmarks.md`](../../docs/benchmarks.md): how to write a benchmark (deterministic,
self-checking, 0.1-0.4 s, the `bench-section-seconds` / `@section` and warm series), and how to
read the bench CI (the bimodal runner pool and the benchmarks that cannot resolve less than ~30%).
Current numbers are read from `bench-history.tsv` on the `bench-data` branch. References in ADRs
and older news entries are left as they were; the file is in git history.
