---
name: reclaim-disk
description: Reclaim disk on a mutsu development box by removing stale agent worktrees and cargo build caches, and optionally set up mold + sccache for faster shared builds. Use when disk is filling up, a build fails with "no space left on device", or worktrees from parallel agents have accumulated.
metadata:
  short-description: Free disk from worktrees and cargo caches
---

# Reclaiming disk

Two things dominate disk use on a mutsu box: agent worktrees under `.claude/worktrees/`, and
cargo build caches under `target/`. Both are disposable — deleting them costs at most a
one-time rebuild.

## Worktree cleanup

`isolation: "worktree"` agents leave stale trees under `.claude/worktrees/` that can consume
hundreds of GB. **Clean them up at least once per hour** during long sessions, and between
batches of the parallel-agent pipeline.

First check which agents are still running (`ListAgents`) and exclude their worktrees. Then:

```bash
cd .claude/worktrees/
for d in agent-*; do
  git -C <repo-root> worktree remove --force ".claude/worktrees/$d" 2>/dev/null
done
git worktree prune
```

Verify with `du -sh .claude/worktrees/`.

## Build cache cleanup

`target/` grows without bound and is usually the top consumer — a single checkout can reach
100 GB+, and several checkouts on one machine easily pass 300 GB. **The dominant offender is
`target/debug/incremental/`**: every branch switch and profile change spawns a new incremental
session, and cargo does not GC old ones aggressively, so it balloons to tens of GB per checkout.

Neither pass below touches source or the built binaries in `target/*/deps` — only regenerable
caches. **First check nothing is building (`pgrep -a cargo rustc`)**, then:

```bash
# 1. Time-based sweep: remove artifacts not touched in 14 days (cargo-sweep).
#    cargo install cargo-sweep   # once
cargo sweep --time 14                 # add --dry-run first to preview

# 2. Nuke incremental caches (the big win). Regenerated on next build.
rm -rf target/*/incremental
```

Run both in *each* checkout on a multi-checkout machine. One such cleanup took the root FS from
84% to 58% (~230 GB freed), ~199 GB of it from `target/debug/incremental`. Consider doing this
monthly, or set `CARGO_INCREMENTAL=0` for checkouts you build in only occasionally so they never
accumulate incremental sessions.

## Optional: faster, shared local builds (mold + sccache)

Not required, but recommended when you keep several checkouts on one machine — their build
caches are otherwise unshared. Configure once in `~/.cargo/config.toml`:

```toml
[build]
rustc-wrapper = "sccache"   # shares compiled dependencies across all checkouts

[target.x86_64-unknown-linux-gnu]
rustflags = ["-C", "link-arg=-fuse-ld=mold"]   # much faster linking
```

Requires `mold` (apt) and `sccache` (`cargo install sccache`; bump its cache with a
`~/.config/sccache/config` `[cache.disk] size` line). sccache passes the *incremental* dev build
of the local crate straight through — so edit→build stays incremental-fast — while caching the
non-incremental dependency and release builds, which is exactly what is shared across checkouts.
CI links with mold and caches dependencies via `Swatinem/rust-cache`; the release profile is
`debug = false` (build `--profile profiling` when you need `perf` symbols).
