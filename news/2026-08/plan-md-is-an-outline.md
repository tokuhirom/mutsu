# PLAN.md is an outline again (1072 → 221 lines)

PLAN.md had grown to 1072 lines. Its own header said it lists "only unfinished work", but in
practice each item had accreted the full history of how it got where it is: measurement tables,
profile snapshots, "Update 2026-07-15 (3)" chains, four-mechanism breakage analyses, and long lists
of PR numbers for work that had already shipped. Finding the actual open question in a section meant
reading several screens of narrative first, and every parallel PR that touched it conflicted.

That is the same problem the per-entry `news/YYYY-MM/` and `todo/` files already solved, applied one
level up. So PLAN.md is now **an outline and nothing else** (user decision, 2026-08-02): each item
says what is left and links to where the detail lives. A table at the top of the file states the
split — completed work in `news/`, open findings in `todo/tickets/` and `todo/deep/`, decisions in
`docs/adr/`, roast analysis in `TODO_roast/BLOCKERS.md`, numbers in the bench CI — and the file
carries an explicit instruction not to append progress notes to it.

Nothing was dropped on the floor. The analysis that was only recorded in PLAN.md moved into new
files, each of which now owns its subject:

- `todo/deep/needs-env-sync-blanket-removal.md` — the `captures_env_by_name` blanket and the four
  independent mechanisms (block-scope restore, cross-thread `cas` capture, method-call caller-local
  coherence × the JIT's inline `GetLocal`, currying) that a standalone removal deterministically
  broke.
- `todo/deep/interpreter-call-path-in-hot-loops.md` — the roast-wide wall-clock re-baseline, the four
  files where mutsu is slower than raku, and why the remaining churn is the named-argument path.
- `todo/deep/shared-worker-pool-adr.md` — the measured groundwork for the pool ADR, including the
  central fork (a bounded pool plus blocking `await` deadlocks, because mutsu has no continuations).
- `todo/tickets/` gained files for the Miri gate that closes ADR-0013, the OTF gate's two remaining
  exclusions, the HTTP::Server::Tiny async remainder, the language-feature gaps that no roast file
  whitelists, and three bugs that were re-verified against `raku` while extracting them: inline
  `start` blocks clobbering a later-declared variable, `&f` re-materializing a fresh `Sub` per
  mention, and a stored regex losing its defining scope's lexicals when it escapes the sub.

Two sections left rather than moved. **§1 B2b (Test::Async)** is gone because Test::Async is not a
bundle candidate — its scouting result (custom Metamodel HOW inheritance is necessary but nowhere
near sufficient; only a narrow per-declarator shim is viable; 8 of 1573 dists depend on it, all
`test-depends`) now lives with the ecosystem survey that classified it, in
`docs/ecosystem-guts-dependency-survey.md`. And the **vendoring mechanism** item was simply stale:
22 batteries are vendored under `modules/` and resolved with zero configuration by
`resolve_bundled_lib_paths`, with `BATTERIES.md` as the policy and a release-time suite gate — that
work is done, so the checkbox went away instead of being re-worded.

The metrics table was re-derived from the tree rather than carried forward: 22 bundled libraries (it
still claimed 0), the roast whitelist at 1435/1464, and binary distribution shipping rather than
"none". `ANALYSIS.md` and `docs/mzef-install-pipeline.md` were updated to point at the new homes
instead of the old section numbers.
