# The `needs_env_sync` blanket removal is complete; the tracking ticket was stale

`todo/deep/needs-env-sync-blanket-removal.md` was extracted from PLAN.md on 2026-08-02 to
record why removing the `captures_env_by_name` frame-wide blanket was a large, fused
campaign. The very next day, PR #5759 ("vm: record env consumer slot sets") and the rest
of the env-writeback-slot campaign implemented exactly that: `CompiledCode` now records
precise per-consumer slot sets (`EnvConsumerSlots` for `ForLoop`, `BlockScope`,
`BlockLocalScope`, `MakeGather`, `WheneverScope`), `compute_needs_env_sync` builds a
genuine per-slot `Vec<bool>` instead of a per-frame flag, block-scope restore
(`exec_block_scope_op`) is slot-authoritative instead of reconstructing locals from env by
name, and closure capture uses slot-addressed shared cells. This was accepted as
[ADR-0018](../../docs/adr/0018-slot-addressed-lexical-capture-and-env-sync.md), whose
migration section records completion on 2026-08-03, and is confirmed done in
`ANALYSIS.md` §1.3 ("Lexical-scope slots — precise per-slot synchronization complete").

`captures_env_by_name` no longer exists anywhere in `src/`. The remaining
`needs_env_sync.iter_mut().for_each(|b| *b = true)` call in `opcode.rs` is a narrow,
intentional escape hatch limited to frames with a lazy body, a resume-control install, an
interpolating regex, or a dynamic substitution replacement — not the general per-frame
blanket the ticket described.

The `todo/deep/` file and its PLAN.md §4 pointer were never cleaned up after the campaign
closed, so the ticket sat as an apparently-open "fused campaign, don't attempt solo"
warning for ten days after it had already been resolved. Removed the stale ticket and its
PLAN.md reference.
