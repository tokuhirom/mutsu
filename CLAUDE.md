# mutsu - Raku compat interpreter (Rust)

## Build & Test

    cargo build                  # build
    cargo test                   # unit tests
    cargo build && prove -e 'target/debug/mutsu' t/   # integration tests
    cargo build && prove -e 'timeout 30 target/debug/mutsu' $(cat roast-whitelist.txt)  # roast tests

Single roast test:

    cargo build && timeout 30 target/debug/mutsu roast/S03-operators/names.t

## Project structure

- `src/` — Rust source (parser, interpreter, builtins)
- `t/` — integration tests (TAP format)
- `roast/` — upstream Raku roast test suite (read-only; do not modify)
- `roast-whitelist.txt` — tests that currently pass; new passes are appended here
- `scripts/pick-next-roast.sh` — picks next failing roast tests to work on
- `tmp/` — roast run results (roast-panic.txt, roast-timeout.txt, roast-error.txt, roast-fail.txt)

## Coding conventions

- Rust 2024 edition
- Keep changes minimal and focused — fix only what is needed to pass the target test
- Do not modify files under `roast/` — those are upstream
- After fixing a roast test, append the test path to `roast-whitelist.txt` (keep sorted)
- Commit messages: short summary + which roast test(s) now pass
- PR title format: `Fix roast/<path>` or `Support <feature>; pass roast/<path>`

## roast fix workflow

When the user says **"roast fix"**, execute this automated loop:

1. Run `scripts/pick-next-roast.sh -n 3` to find the next failing roast tests
2. For each test, spawn a **sub-agent** (Task tool, subagent_type=Bash) that:
   - Creates a git worktree: `git worktree add .git/worktrees/<branch-name> -b <branch-name> main`
   - Works in that worktree directory to fix the interpreter so the roast test passes
   - Runs `cargo build && timeout 30 target/debug/mutsu <roast-test-path>` to verify
   - Appends the test path to `roast-whitelist.txt` (sorted)
   - Commits, pushes the branch, and creates a PR with `gh pr create`
   - Polls PR CI status every 60 seconds with `gh pr checks <pr-number>`
   - If CI fails: reads the failure log, pushes fix commits, and retries
   - If CI passes: merges the PR with `gh pr merge <pr-number> --merge`
   - Cleans up the worktree: `git worktree remove .git/worktrees/<branch-name>`
3. When sub-agents finish, pull main (`git pull`), then repeat from step 1
4. Continue this loop indefinitely until stopped by the user
