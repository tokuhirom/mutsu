---
name: worktree-build-lock-fix
description: Worktree agents may share the main target/ dir causing build lock contention - add CARGO_TARGET_DIR or verify Cargo.toml exists
type: feedback
---

When spawning worktree agents, some worktrees may not have Cargo.toml checked out, causing cargo to use the main repo's target/ directory and creating build lock contention across all agents.

**Why:** git worktrees sometimes don't fully checkout files. Without Cargo.toml in the worktree, cargo traverses up to find the main repo's workspace root and uses its shared target/ dir.

**How to apply:** In worktree agent prompts, add this instruction:
"Before building, verify Cargo.toml exists in your working directory. If not, run `git checkout HEAD -- .` to ensure all files are checked out."
