# Repository Guidelines for Codex

`mutsu` is a Rust implementation of a Raku compatibility interpreter. Repository
artifacts (code, tests, documentation, commits, and PR text) must be in English.

## Start Here

Before planning or changing code, read this file in full and then read the
task-relevant primary material: the selected issue, its linked ADRs and design
documents, and the affected code/tests. Re-check ADR status lines rather than
relying on an old issue's description.

The open-findings backlog is **GitHub issues on `tokuhirom/mutsu`**, labelled
`todo:ticket` / `todo:deep` / `todo:perf`; `docs/issue-workflow.md` is the
operating manual and `docs/triage.md` ranks the backlog. Only ever file, label,
comment on or close issues in `tokuhirom/mutsu` — never in any other
repository. Agents run in parallel, so **claim an issue before starting it**:
post `Claiming: <your-branch-name>`, re-read the comments and yield if an older
live claim is there, then add the `working` label; post
`Releasing: <your-branch-name>` and drop the label when you finish. Skip any
issue that already carries `working` or a live claim.

For a request to implement a `todo:ticket` issue, use the `mutsu-ticket-flow`
skill. It defines the required lifecycle through a verified merge and selection
of the next ticket.

A request to work a *slice* of that queue ("the `tier:N` tickets", "the ones
with no tier yet", "one after another, open the PRs") is a complete standing
instruction, not the start of a negotiation. The mechanics are already settled
— one issue per PR, oldest-first within the named slice, claim before starting,
a regression test and a `news/` entry per fix, auto-merge enabled, straight on
to the next after each verified merge — so do not ask them back. Raise only a
decision that is genuinely the user's, and raise it in the final report while
the rest of the batch continues.

Self-contained procedures live under `.agents/skills/<name>/SKILL.md` rather
than in this file; read the matching one before starting such a task. Currently:
`cut-release` (releasing), `install-raku` (installing the Rakudo oracle when
`raku` is missing), `roast-triage` (choosing and investigating roast work),
`test-util-workout`, `reclaim-disk` (stale worktrees and cargo caches),
`mutsu-ticket-flow`, and `rakuast-implementation`.

## Architecture

The execution pipeline is Parser -> Compiler -> VM. Implement language features
as parser/compiler/VM behavior. Do not add a new interpreter or runtime
slow-path fallback from VM code (including calls such as
`call_method_with_values`, `run_instance_method`, or `eval_block`); existing
fallbacks are debt, not a precedent. Prefer an opcode plus compiler and VM
support when an operation needs new execution behavior.

Key directories:

- `src/parser/`, `src/compiler/`, `src/vm/`: execution pipeline.
- `src/builtins/`, `src/value/`: native behavior and values.
- `src/runtime/`: remaining dispatch/runtime machinery.
- `t/`: local TAP integration tests.
- `tests/`: Rust-driven TAP tests.
- `roast/`: read-only upstream specification tests.
- `docs/adr/`: architectural decisions; read applicable ADRs before changing
  their area.
- `docs/issue-workflow.md`: how the GitHub-issue work queue is run.
  `docs/todo-issue-map.md` resolves the `todo/...md` paths older records cite.

Do not implement ecosystem modules as native replacements. Grow the interpreter
so vendored upstream modules run unchanged, unless the user explicitly approves
an exception. Do not confuse helpers supplied by a test module with Raku core
builtins; verify core routines against both a bare `raku` invocation and the
Raku documentation.

## Development and Tests

- `cargo build`: build `target/debug/mutsu`.
- `cargo test`: Rust tests.
- `make test`: Rust tests, build, and local TAP tests; its log is
  `tmp/make-test.log`.
- `make roast`: whitelisted roast tests; its log is `tmp/make-roast.log`.
- `cargo fmt --all`: format Rust.
- `cargo clippy -- -D warnings`: lint with warnings denied.

Add a focused regression test for each behavior change, normally under `t/`.
Run a targeted test while iterating. Before publishing a code PR, run
`cargo fmt --all`, `cargo clippy -- -D warnings`, `make test`, and `make roast`
once each. After either full command runs, inspect its saved log. A failing
full test belongs to the branch: diagnose it and use targeted checks as needed
for further evidence. Keep `roast-whitelist.txt` sorted when changing it.

Documentation-only PRs do not require Rust formatting, linting, or either full
test suite. Re-triaging an issue (relabelling `todo:ticket` to `todo:deep`,
adding an investigation comment) touches no files at all and needs neither.
Verify a documentation patch with `git diff --check` and run a focused check
only when it affects generated output, executable scripts, or test
configuration.

Full-suite reruns are allowed when evidence requires them, but never run the
same full suite concurrently. The suites share Cargo build locks, temporary
logs, and test-harness state, so overlapping invocations can create spurious
failures and invalid logs.

For an individual roast test, use `MUTSU_FUDGE=1`; do not set that variable for
ordinary Raku programs. Do not modify `roast/`, `raku-doc/`, or other upstream
submodules. Initialize missing submodules with
`git submodule update --init --recursive`.

Use rustfmt defaults and standard Rust naming (`snake_case` functions/modules,
`CamelCase` types, `SCREAMING_SNAKE_CASE` constants). Avoid unrelated rewrites,
hardcoded outputs, stubs, and early-return test workarounds. Use ephemeral test
files only under the gitignored `tmp/` directory.

## Git and Pull Requests

Never commit to `main`; create a focused feature branch. Preserve unrelated
working-tree changes. Do not use destructive Git operations to discard work.
Use conventional PR titles such as `fix: ...` or `parser: ...`.

After validation, proactively commit, push, and open a ready-for-review PR.
Enable auto-merge using merge or rebase (not squash). Immediately verify that it
is ready and auto-merge is enabled, then check its merge state:

```sh
gh pr view <number> --json isDraft,autoMergeRequest,mergeStateStatus,state
gh pr view <number> --json mergeStateStatus,state -q '.state + " / " + .mergeStateStatus'
```

If it is `DIRTY`, rebase onto `origin/main`, resolve it, and force-push with
lease. Monitor required checks with `gh pr checks <number> --watch --fail-fast`.
Fix failures forward on the same branch, push, and monitor again. A PR is not
complete merely because checks passed or auto-merge was requested: verify GitHub
reports `state == MERGED`, and verify its merge commit is reachable from
`origin/main` before reporting completion or taking the next ticket.

Do not create stacked PRs or close a PR simply to discard its work. Do not open
PRs or issues against any repository other than `tokuhirom/mutsu` — above all
not a Raku organization one (`roast`, `raku-doc`, `rakudo`), where an AI has
actually mis-filed a mutsu issue before.

Some sessions have no `gh` (ephemeral remote containers generally do not, and
direct `api.github.com` calls from them are rejected by the session proxy).
There, use the GitHub MCP tools with `owner: tokuhirom`, `repo: mutsu` for the
same steps; `docs/agent-environments.md` maps each `gh` command above to its
tool, and records the other differences between the two environments (cores,
disk, provisioning) — `git` itself and every build/test command are identical.
