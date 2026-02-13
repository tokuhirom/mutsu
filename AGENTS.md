# AGENTS

This repo is a Rust implementation of a minimal Raku (Perl 6) compatible interpreter.

## Working agreements
- Keep changes small and well-documented
- Perl 6 (Raku) regex is not compatible with Perl 5 regex; never assume Perl 5 compatibility.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.
- Commit directly to the main branch. Do not use feature branches.
- Push to remote after completing work.
- Write all documents (CLAUDE.md, TODO.md, etc.), code comments, and commit messages in English.

## Layout
- `src/` holds the interpreter implementation.
- `bin/` holds the CLI entrypoint (if needed).
- `tests/` holds local Rust tests (unit/integration).

## Build & run
- Build: `cargo build`
- Run: `cargo run -- <file.p6>`
- Test: `cargo test`
- Full test (cargo + prove): `make test`
- Temporary test scripts: write to `tmp/` in the project root using the Write tool (not cat/heredoc). The `tmp/` directory is gitignored.
- Do not use `echo`, `cat`, `printf`, or heredoc via Bash to create files. Always use the Write tool.
- When verifying behavior manually, build first with `cargo build`, then run with `./target/debug/mutsu ./tmp/<file>`.

## Reference implementation
- The reference Raku implementation is available as `raku` on this system.
- Use `raku -e '<code>'` to check expected behavior when the spec is unclear.

## Spec sources
- Design docs live at `./old-design-docs/`.
- The official Raku test suite (roast) is available at `./roast/` as a git submodule.

## Roast (official Raku test suite)
- The ultimate goal is to pass ALL roast tests. The order in which tests are fixed does not matter — pick any failing test and work on it.
- `roast/` contains the upstream Raku spec tests. It is read-only; never modify files under `roast/`.
- `TODO_roast.md` tracks per-file pass/fail status. Mark a test `[x]` only when **all** of its subtests pass.
- When a test file has known partial failures, add indented notes under its entry describing the blockers.
- `roast-whitelist.txt` lists tests that pass completely. `make roast` runs only these via prove.
- After fixing a bug or adding a feature, check whether any roast tests now pass and update the whitelist and TODO accordingly.
- Do not add a roast test to the whitelist unless `prove -e 'cargo run --' <file>` exits cleanly (all subtests pass).
- Roast tests may use constructs the interpreter does not yet support. Prefer fixing the interpreter over skipping tests.
- Never add special-case logic, hardcoded results, or test-specific hacks just to pass a roast test. Every fix must be a genuine, general-purpose improvement to the interpreter.
- When the expected behavior of a roast test is unclear, consult `./old-design-docs/` for the original Raku language specification before implementing.
- When investigating a roast test and deciding to defer it, always record the reason for failure in `TODO_roast.md` under the corresponding entry. Even if a test is not fixed now, the diagnosis is valuable for future work.
- Roast is the authoritative spec. If passing a roast test requires changing a local test under `t/`, update the local test to match roast's expected behavior.
- When `make roast` shows failures in whitelisted tests, do NOT dismiss them as "pre-existing". Investigate each failure, fix what can be fixed, and remove from the whitelist only tests that truly cannot pass yet (with documented reasons in TODO_roast.md).
- When a roast test requires solving multiple unrelated prerequisite problems that go beyond the test's main topic, fix what you can, update `TODO_roast.md` with the diagnosis, and move on to the next test. Do not get stuck on a single test.

## Roast test prioritization
- Run `./scripts/roast-history.sh` to generate per-file category lists under `tmp/`:
  - `tmp/roast-panic.txt` — tests that cause a Rust panic (interpreter crash)
  - `tmp/roast-timeout.txt` — tests that exceed the timeout limit
  - `tmp/roast-error.txt` — tests that produce no valid TAP plan (parse/runtime error before TAP output)
  - `tmp/roast-fail.txt` — tests with some subtests failing
  - `tmp/roast-pass.txt` — tests that pass completely
- When choosing which roast tests to work on, follow this priority order:
  1. **Panic** (highest priority): These indicate interpreter bugs that crash the process. Fix these first.
  2. **Timeout**: These may indicate infinite loops or severe performance issues. Fix these second.
  3. **Error / Fail** (remaining): Pick from these in any order. Prefer tests that seem close to passing (few failing subtests) or that exercise widely-used features.
- The category files are regenerated each time the script runs, so always use the latest output.

## Checking `make roast` results
- To find failing tests in `make roast` output:
  ```
  make roast 2>&1 | grep -E "(FAILED|Failed)" | head -20
  ```
- To see detailed failure info:
  ```
  make roast 2>&1 | grep -E "(not ok|FAILED|Failed|Wstat)" | head -20
  ```

## Parallel agents with git worktrees
- When running multiple sub-agents in parallel that modify source code, each agent MUST work in its own git worktree to avoid file conflicts.
- Create worktrees under `.git/worktrees-work/` (not `/tmp`):
  ```
  git worktree add .git/worktrees-work/<name> -b worktree/<name> HEAD
  ```
- Each sub-agent works, builds, and tests entirely within its own worktree directory. It does NOT commit or push.
- When a sub-agent finishes, the parent agent integrates its changes immediately (don't wait for all agents). After integrating, launch a new agent for the next task to keep the pipeline full.
- To integrate: export diff from worktree, apply to main repo, fix clippy/fmt, run `make roast`, commit and push.
- After integration, clean up the finished worktree:
  ```
  git worktree remove .git/worktrees-work/<name>
  git branch -d worktree/<name>
  ```

## Process optimization
- After integrating a sub-agent's work, review its processing log for optimization opportunities (e.g., repeated failures, unnecessary steps, patterns that could be avoided). Apply improvements to CLAUDE.md or agent prompts.
- Always ensure cwd is the main repo (`/home/tokuhirom/work/mutsu`) before running `make roast`, `git commit`, or `git push`. Worktree cwd can leak between commands.
- When applying patches from worktrees, prefer manual edits over `git apply` since the worktree base commit differs from main HEAD after incremental integration.

## Debugging guidelines
- Do NOT use printf debugging (adding eprintln! → build → check → repeat). Rust builds are slow and this wastes time.
- Instead, use these approaches in order of preference:
  1. **Trace logs**: Run with `MUTSU_TRACE=1 ./target/debug/mutsu <file>` to see execution flow. Use `MUTSU_TRACE=eval` or `MUTSU_TRACE=parse,vm` to filter by phase.
  2. **Focused unit tests**: Write a small `#[test]` in the relevant module to isolate the problem. `cargo test <name>` is fast with incremental compilation.
  3. **Read the code**: Trace the logic by reading, not by running. Many bugs are visible from careful code review.
  4. **Debugger**: Use `rust-gdb ./target/debug/mutsu` with breakpoints for complex state inspection.
- If you must add temporary debug prints, add ALL of them in one pass before building — never do build-run cycles for individual print statements.
- Always remove debug prints before committing.

## Conventions
- Add small, focused tests for each new syntax feature.
- Keep the parser and evaluator readable; comment only non-obvious logic.
- Write tests using prove. Do not use Rust integration tests in `tests/*.rs` for new coverage.
- Every feature addition must include tests. A feature without tests is considered incomplete.
